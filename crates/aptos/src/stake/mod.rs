// Copyright (c) Aptos
// SPDX-License-Identifier: Apache-2.0

use crate::common::types::{
    CliCommand, CliError, CliResult, CliTypedResult, StakePoolType, TransactionOptions,
    TransactionSummary,
};
use crate::common::utils::get_stake_pool_type;
use aptos_types::account_address::{create_vesting_pool_address, AccountAddress};
use async_trait::async_trait;
use cached_packages::aptos_stdlib;
use clap::Parser;

/// Tool for manipulating stake
///
#[derive(Parser)]
pub enum StakeTool {
    AddStake(AddStake),
    UnlockStake(UnlockStake),
    WithdrawStake(WithdrawStake),
    IncreaseLockup(IncreaseLockup),
    InitializeStakeOwner(InitializeStakeOwner),
    SetOperator(SetOperator),
    SetDelegatedVoter(SetDelegatedVoter),
}

impl StakeTool {
    pub async fn execute(self) -> CliResult {
        use StakeTool::*;
        match self {
            AddStake(tool) => tool.execute_serialized().await,
            UnlockStake(tool) => tool.execute_serialized().await,
            WithdrawStake(tool) => tool.execute_serialized().await,
            IncreaseLockup(tool) => tool.execute_serialized().await,
            InitializeStakeOwner(tool) => tool.execute_serialized().await,
            SetOperator(tool) => tool.execute_serialized().await,
            SetDelegatedVoter(tool) => tool.execute_serialized().await,
        }
    }
}

/// Stake APT coins to the stake pool
///
/// This command allows stake pool owners to add APT coins to their stake.
#[derive(Parser)]
pub struct AddStake {
    /// Amount of Octas (10^-8 APT) to add to stake
    #[clap(long)]
    pub amount: u64,

    /// Required to identify which stake pool if the staker has staking contracts with operators or
    /// administrates multiple vesting accounts..
    #[clap(long)]
    pub operator: Option<AccountAddress>,

    #[clap(flatten)]
    pub(crate) txn_options: TransactionOptions,
}

#[async_trait]
impl CliCommand<TransactionSummary> for AddStake {
    fn command_name(&self) -> &'static str {
        "AddStake"
    }

    async fn execute(mut self) -> CliTypedResult<TransactionSummary> {
        let owner = self.txn_options.profile_options.account_address()?;
        let payload =
            match get_stake_pool_type(&self.txn_options.rest_client().unwrap(), owner).await {
                StakePoolType::Direct => aptos_stdlib::stake_add_stake(self.amount),
                StakePoolType::StakingContract => {
                    aptos_stdlib::staking_contract_add_stake(self.operator.unwrap(), self.amount)
                }
                _ => {
                    return Err(CliError::Unsupported(
                        "Adding stake is only supported for direct stake pool or staking contract"
                            .into(),
                    ))
                }
            };
        self.txn_options
            .submit_transaction(payload)
            .await
            .map(|inner| inner.into())
    }
}

/// Unlock staked APT coins
///
/// APT coins can only be unlocked if they no longer have an applied lockup period
#[derive(Parser)]
pub struct UnlockStake {
    /// Amount of Octas (10^-8 APT) to unlock
    #[clap(long)]
    pub amount: u64,

    /// Required to identify which stake pool if the staker has staking contracts with operators or
    /// administrates multiple vesting accounts..
    #[clap(long)]
    pub operator: Option<AccountAddress>,

    #[clap(flatten)]
    pub(crate) txn_options: TransactionOptions,
}

#[async_trait]
impl CliCommand<TransactionSummary> for UnlockStake {
    fn command_name(&self) -> &'static str {
        "UnlockStake"
    }

    async fn execute(mut self) -> CliTypedResult<TransactionSummary> {
        let owner = self.txn_options.profile_options.account_address()?;
        let payload =
            match get_stake_pool_type(&self.txn_options.rest_client().unwrap(), owner).await {
                StakePoolType::Direct => aptos_stdlib::stake_unlock(self.amount),
                StakePoolType::StakingContract => {
                    aptos_stdlib::staking_contract_unlock_stake(self.operator.unwrap(), self.amount)
                }
                _ => return Err(CliError::Unsupported(
                    "Unlocking stake is only supported for direct stake pool or staking contract"
                        .into(),
                )),
            };
        self.txn_options
            .submit_transaction(payload)
            .await
            .map(|inner| inner.into())
    }
}

/// Withdraw unlocked staked APT coins
///
/// This allows users to withdraw stake back into their CoinStore.
/// Before calling `WithdrawStake`, `UnlockStake` must be called first.
#[derive(Parser)]
pub struct WithdrawStake {
    /// Amount of Octas (10^-8 APT) to withdraw
    #[clap(long)]
    pub amount: u64,

    /// Required to identify which stake pool if the staker has staking contracts with operators or
    /// administrates multiple vesting accounts..
    #[clap(long)]
    pub operator: Option<AccountAddress>,

    /// Required to identify which vesting contract if the owner administrates multiples.
    #[clap(long)]
    pub vesting_contract_index: Option<u64>,

    #[clap(flatten)]
    pub(crate) txn_options: TransactionOptions,
}

#[async_trait]
impl CliCommand<TransactionSummary> for WithdrawStake {
    fn command_name(&self) -> &'static str {
        "WithdrawStake"
    }

    async fn execute(mut self) -> CliTypedResult<TransactionSummary> {
        let owner = self.txn_options.profile_options.account_address()?;
        let payload =
            match get_stake_pool_type(&self.txn_options.rest_client().unwrap(), owner).await {
                StakePoolType::Direct => aptos_stdlib::stake_withdraw(self.amount),
                StakePoolType::StakingContract => {
                    aptos_stdlib::staking_contract_distribute(owner, self.operator.unwrap())
                }
                StakePoolType::Vesting => {
                    let vesting_contract_address = create_vesting_pool_address(
                        owner,
                        self.operator.unwrap(),
                        self.vesting_contract_index.unwrap(),
                        &[],
                    );
                    aptos_stdlib::vesting_distribute(vesting_contract_address)
                }
                _ => {
                    return Err(CliError::ConfigNotFoundError(
                        "Account has no stake pool associated".into(),
                    ))
                }
            };
        self.txn_options
            .submit_transaction(payload)
            .await
            .map(|inner| inner.into())
    }
}

/// Increase lockup of all staked APT coins in the stake pool
///
/// Lockup may need to be increased in order to vote on a proposal.
#[derive(Parser)]
pub struct IncreaseLockup {
    #[clap(flatten)]
    pub(crate) txn_options: TransactionOptions,

    /// Required to identify which stake pool if the staker has staking contracts with operators or
    /// administrates multiple vesting accounts.
    #[clap(long)]
    pub operator: Option<AccountAddress>,

    /// Required to identify which vesting contract if the owner administrates multiples.
    #[clap(long)]
    pub vesting_contract_index: Option<u64>,
}

#[async_trait]
impl CliCommand<TransactionSummary> for IncreaseLockup {
    fn command_name(&self) -> &'static str {
        "IncreaseLockup"
    }

    async fn execute(mut self) -> CliTypedResult<TransactionSummary> {
        let owner = self.txn_options.profile_options.account_address()?;
        let payload =
            match get_stake_pool_type(&self.txn_options.rest_client().unwrap(), owner).await {
                StakePoolType::Direct => aptos_stdlib::stake_increase_lockup(),
                StakePoolType::StakingContract => {
                    aptos_stdlib::staking_contract_reset_lockup(self.operator.unwrap())
                }
                StakePoolType::Vesting => {
                    let vesting_contract_address = create_vesting_pool_address(
                        owner,
                        self.operator.unwrap(),
                        self.vesting_contract_index.unwrap(),
                        &[],
                    );
                    aptos_stdlib::vesting_reset_lockup(vesting_contract_address)
                }
                _ => {
                    return Err(CliError::ConfigNotFoundError(
                        "Account has no stake pool associated".into(),
                    ))
                }
            };
        self.txn_options
            .submit_transaction(payload)
            .await
            .map(|inner| inner.into())
    }
}

/// Initialize stake owner
///
/// Initializing stake owner adds the capability to delegate the
/// stake pool to an operator, or delegate voting to a different account.
#[derive(Parser)]
pub struct InitializeStakeOwner {
    /// Initial amount of Octas (10^-8 APT) to be staked
    #[clap(long)]
    pub initial_stake_amount: u64,

    /// Account Address of delegated operator
    ///
    /// If not specified, it will be the same as the owner
    #[clap(long, parse(try_from_str=crate::common::types::load_account_arg))]
    pub operator_address: Option<AccountAddress>,

    /// Account address of delegated voter
    ///
    /// If not specified, it will be the same as the owner
    #[clap(long, parse(try_from_str=crate::common::types::load_account_arg))]
    pub voter_address: Option<AccountAddress>,

    #[clap(flatten)]
    pub(crate) txn_options: TransactionOptions,
}

#[async_trait]
impl CliCommand<TransactionSummary> for InitializeStakeOwner {
    fn command_name(&self) -> &'static str {
        "InitializeStakeOwner"
    }

    async fn execute(mut self) -> CliTypedResult<TransactionSummary> {
        let owner_address = self.txn_options.sender_address()?;
        self.txn_options
            .submit_transaction(aptos_stdlib::stake_initialize_stake_owner(
                self.initial_stake_amount,
                self.operator_address.unwrap_or(owner_address),
                self.voter_address.unwrap_or(owner_address),
            ))
            .await
            .map(|inner| inner.into())
    }
}

/// Delegate operator capability from the stake owner to another account
#[derive(Parser)]
pub struct SetOperator {
    /// Account Address of delegated operator
    ///
    /// If not specified, it will be the same as the owner
    #[clap(long, parse(try_from_str=crate::common::types::load_account_arg))]
    pub operator_address: AccountAddress,

    /// Required to identify which stake pool if the staker has staking contracts with operators.
    #[clap(long)]
    pub old_operator: Option<AccountAddress>,

    /// Required to identify which vesting contract if the owner administrates multiples.
    #[clap(long)]
    pub vesting_contract_index: Option<u64>,

    #[clap(flatten)]
    pub(crate) txn_options: TransactionOptions,
}

#[async_trait]
impl CliCommand<TransactionSummary> for SetOperator {
    fn command_name(&self) -> &'static str {
        "SetOperator"
    }

    async fn execute(mut self) -> CliTypedResult<TransactionSummary> {
        let owner = self.txn_options.profile_options.account_address()?;
        let operator = self.operator_address;
        let payload =
            match get_stake_pool_type(&self.txn_options.rest_client().unwrap(), owner).await {
                StakePoolType::Direct => aptos_stdlib::stake_set_operator(operator),
                StakePoolType::StakingContract => {
                    aptos_stdlib::staking_contract_switch_operator_with_same_commission(
                        self.old_operator.unwrap(),
                        operator,
                    )
                }
                StakePoolType::Vesting => {
                    let vesting_contract_address = create_vesting_pool_address(
                        owner,
                        operator,
                        self.vesting_contract_index.unwrap(),
                        &[],
                    );
                    aptos_stdlib::vesting_update_operator_with_same_commission(
                        vesting_contract_address,
                        operator,
                    )
                }
                _ => {
                    return Err(CliError::ConfigNotFoundError(
                        "Account has no stake pool associated".into(),
                    ))
                }
            };
        self.txn_options
            .submit_transaction(payload)
            .await
            .map(|inner| inner.into())
    }
}

/// Delegate voting capability from the stake owner to another account
#[derive(Parser)]
pub struct SetDelegatedVoter {
    /// Account Address of delegated voter
    ///
    /// If not specified, it will be the same as the owner
    #[clap(long, parse(try_from_str=crate::common::types::load_account_arg))]
    pub voter_address: AccountAddress,

    /// Required to identify which stake pool if the staker has staking contracts with operators.
    #[clap(long)]
    pub operator: Option<AccountAddress>,

    /// Required to identify which vesting contract if the owner administrates multiples.
    #[clap(long)]
    pub vesting_contract_index: Option<u64>,

    #[clap(flatten)]
    pub(crate) txn_options: TransactionOptions,
}

#[async_trait]
impl CliCommand<TransactionSummary> for SetDelegatedVoter {
    fn command_name(&self) -> &'static str {
        "SetDelegatedVoter"
    }

    async fn execute(mut self) -> CliTypedResult<TransactionSummary> {
        let owner = self.txn_options.profile_options.account_address()?;
        let voter = self.voter_address;
        let payload =
            match get_stake_pool_type(&self.txn_options.rest_client().unwrap(), owner).await {
                StakePoolType::Direct => aptos_stdlib::stake_set_delegated_voter(voter),
                StakePoolType::StakingContract => {
                    aptos_stdlib::staking_contract_update_voter(self.operator.unwrap(), voter)
                }
                StakePoolType::Vesting => {
                    let vesting_contract_address = create_vesting_pool_address(
                        owner,
                        self.operator.unwrap(),
                        self.vesting_contract_index.unwrap(),
                        &[],
                    );
                    aptos_stdlib::vesting_update_voter(vesting_contract_address, voter)
                }
                _ => {
                    return Err(CliError::ConfigNotFoundError(
                        "Account has no stake pool associated".into(),
                    ))
                }
            };
        self.txn_options
            .submit_transaction(payload)
            .await
            .map(|inner| inner.into())
    }
}
