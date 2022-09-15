// Copyright (c) Aptos
// SPDX-License-Identifier: Apache-2.0

use crate::{generate_traffic, LoadDestination, NetworkLoadTest};
use aptos::account::create::DEFAULT_FUNDED_COINS;
use aptos_genesis::config::HostAndPort;
use aptos_logger::info;
use aptos_sdk::crypto::ed25519::Ed25519PrivateKey;
use aptos_sdk::crypto::{bls12381, x25519};
use forge::{
    reconfig, NetworkContext, NetworkTest, NodeExt, Result, Swarm, SwarmExt, Test, FORGE_KEY_SEED,
};

use aptos_keygen::KeyGen;

use aptos::test::CliTestFramework;
use aptos_sdk::types::network_address::DnsName;
use std::time::Duration;
use tokio::runtime::Runtime;

const MAX_NODE_LAG_SECS: u64 = 10;

pub struct ValidatorJoinLeaveTest;

impl Test for ValidatorJoinLeaveTest {
    fn name(&self) -> &'static str {
        "validator join and leave sets"
    }
}

impl NetworkLoadTest for ValidatorJoinLeaveTest {
    fn setup(&self, _ctx: &mut NetworkContext) -> Result<LoadDestination> {
        Ok(LoadDestination::AllValidators)
    }

    fn test(&self, swarm: &mut dyn Swarm, duration: Duration) -> Result<()> {
        // Verify we have at least 7 validators (i.e., 3f+1, where f is 2)
        // so we can kill 2 validators but still make progress.
        let all_validators = swarm.validators().map(|v| v.peer_id()).collect::<Vec<_>>();
        let num_validators = all_validators.len();
        if num_validators < 7 {
            return Err(anyhow::format_err!(
                "State sync validator performance tests require at least 7 validators! Given: {:?} \
                 This is to ensure the chain can still make progress when 2 validators are killed.",
                num_validators
            ));
        }

        let faucet_endpoint: reqwest::Url = "http://localhost:8081".parse().unwrap();
        // Connect the operator tool to the node's JSON RPC API
        let rest_client = swarm.validators().next().unwrap().rest_client();
        let transaction_factory = swarm.chain_info().transaction_factory();
        let runtime = Runtime::new().unwrap();

        let mut cli = runtime.block_on(async {
            CliTestFramework::new(
                swarm.validators().next().unwrap().rest_api_endpoint(),
                faucet_endpoint,
                /*num_cli_accounts=*/ 0,
            )
            .await
        });

        // Add the root account to cli
        let root_cli_index =
            cli.add_account_to_cli(swarm.chain_info().root_account().private_key().clone());

        println!("root address: {}", swarm.chain_info().root_account().address());

        let mut validator_cli_indices = Vec::new();

        let port = 1234;
        let starting_seed_in_decimal = i64::from_str_radix(FORGE_KEY_SEED, 16)?;

        for i in 0..num_validators {
            // Initialize keyGen to get validator private keys. We uses the same seed in the test
            // driver as in the genesis script so that the validator keys are deterministic.
            let mut seed_slice = [0u8; 32];
            let seed_in_decimal = starting_seed_in_decimal + (i as i64);
            let seed_in_hex_string = format!("{seed_in_decimal:0>64x}");
            println!("Seed in hex: {}", seed_in_hex_string);
            hex::decode_to_slice(seed_in_hex_string, &mut seed_slice)?;

            for x in seed_slice {
                println!(".............{}", x);
            }

            let mut keygen = KeyGen::from_seed(seed_slice);





            let (validator_cli_index, keys) = runtime.block_on(async {
                let (validator_cli_index, keys) =
                    init_validator_account(&mut cli, &mut keygen).await;

                cli.transfer_coins(
                    root_cli_index,
                    validator_cli_index,
                    /*amount=*/ DEFAULT_FUNDED_COINS,
                    None,
                )
                .await
                .unwrap();
                cli.assert_account_balance_now(
                    validator_cli_index,
                    DEFAULT_FUNDED_COINS,
                )
                .await;
                (validator_cli_index, keys)
            });
            validator_cli_indices.push(validator_cli_index);

            assert_eq!(
                runtime.block_on(async { get_validator_state(&cli, validator_cli_index).await }),
                ValidatorState::ACTIVE
            );
        }

        // Log the test setup
        info!(
            "Running validator join and leave test {:?} with {:?} validators.",
            self.name(),
            num_validators,
        );

        // Wait for all nodes to synchronize and stabilize.
        info!("Waiting for the validators to be synchronized.");
        runtime.block_on(async {
            swarm
                .wait_for_all_nodes_to_catchup(Duration::from_secs(MAX_NODE_LAG_SECS))
                .await
        })?;

        // Wait for 1/3 of the test duration.
        std::thread::sleep(duration / 3);

        // 1/3 validators leave the validator set.
        info!("Make 1/3 validators leave the validator set!");
        for operator_index in validator_cli_indices.iter().take(num_validators / 3) {
            runtime.block_on(async {
                cli.leave_validator_set(*operator_index, None)
                    .await
                    .unwrap();

                reconfig(
                    &rest_client,
                    &transaction_factory,
                    swarm.chain_info().root_account(),
                )
                .await
            });
        }

        // Wait for 1/3 of the test duration.
        std::thread::sleep(duration / 3);

        // Rejoining validator set.
        for operator_index in validator_cli_indices.iter().take(num_validators / 3) {
            runtime.block_on(async {
                cli.join_validator_set(*operator_index, None).await.unwrap();

                reconfig(
                    &rest_client,
                    &transaction_factory,
                    swarm.chain_info().root_account(),
                )
                .await
            });
        }

        // Wait for all nodes to synchronize and stabilize.
        info!("Waiting for the validators to be synchronized.");
        runtime.block_on(async {
            swarm
                .wait_for_all_nodes_to_catchup(Duration::from_secs(MAX_NODE_LAG_SECS))
                .await
        })?;

        Ok(())
    }
}

impl NetworkTest for ValidatorJoinLeaveTest {
    fn run<'t>(&self, ctx: &mut NetworkContext<'t>) -> Result<()> {
        <dyn NetworkLoadTest>::run(self, ctx)
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ValidatorState {
    ACTIVE,
    JOINING,
    LEAVING,
    NONE,
}

struct ValidatorNodeKeys {
    account_private_key: Ed25519PrivateKey,
    network_private_key: x25519::PrivateKey,
    consensus_private_key: bls12381::PrivateKey,
}

impl ValidatorNodeKeys {
    pub fn new(keygen: &mut KeyGen) -> Self {
        Self {
            account_private_key: keygen.generate_ed25519_private_key(),
            network_private_key: keygen.generate_x25519_private_key().unwrap(),
            consensus_private_key: keygen.generate_bls12381_private_key(),
        }
    }

    pub fn network_public_key(&self) -> x25519::PublicKey {
        self.network_private_key.public_key()
    }

    pub fn consensus_public_key(&self) -> bls12381::PublicKey {
        bls12381::PublicKey::from(&self.consensus_private_key)
    }

    pub fn consensus_proof_of_possession(&self) -> bls12381::ProofOfPossession {
        bls12381::ProofOfPossession::create(&self.consensus_private_key)
    }
}

async fn init_validator_account(
    cli: &mut CliTestFramework,
    keygen: &mut KeyGen,
) -> (usize, ValidatorNodeKeys) {
    let validator_node_keys = ValidatorNodeKeys::new(keygen);
    let validator_cli_index =
        cli.add_account_to_cli(validator_node_keys.account_private_key.clone());
    (validator_cli_index, validator_node_keys)
}

async fn get_validator_state(cli: &CliTestFramework, pool_index: usize) -> ValidatorState {
    let validator_set = cli.show_validator_set().await.unwrap();
    let pool_address = cli.account_id(pool_index);

    for (state, list) in [
        (ValidatorState::ACTIVE, &validator_set.active_validators),
        (ValidatorState::JOINING, &validator_set.pending_active),
        (ValidatorState::LEAVING, &validator_set.pending_inactive),
    ] {
        if list.iter().any(|info| info.account_address == pool_address) {
            return state;
        }
    }
    ValidatorState::NONE
}

fn dns_name(addr: &str) -> DnsName {
    DnsName::try_from(addr.to_string()).unwrap()
}
