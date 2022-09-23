// Copyright (c) Aptos
// SPDX-License-Identifier: Apache-2.0

use aptos_types::account_address::AccountAddress;
use e2e_move_tests::{assert_success, MoveHarness};
use move_deps::move_core_types::parser::parse_struct_tag;
use serde::{Deserialize, Serialize};

mod common;

/// Mimics `0xcafe::test::ModuleData`
#[derive(Serialize, Deserialize)]
struct ModuleData {
    state: Vec<u8>,
}

fn success(tests: Vec<(&str, Vec<(Vec<Vec<u8>>, &str)>)>) {
    let mut h = MoveHarness::new();

    // Load the code
    let acc = h.new_account_at(AccountAddress::from_hex_literal("0xcafe").unwrap());
    assert_success!(h.publish_package(&acc, &common::test_dir_path("string_args.data/pack")));

    let module_data = parse_struct_tag("0xCAFE::test::ModuleData").unwrap();

    // Check in initial state, resource does not exist.
    assert!(!h.exists_resource(acc.address(), module_data.clone()));

    for (entry, in_out) in tests {
        for (args, expected_change) in in_out {
            assert_success!(h.run_entry_function(&acc, str::parse(entry).unwrap(), vec![], args));
            assert_eq!(
                String::from_utf8(
                    h.read_resource::<ModuleData>(acc.address(), module_data.clone())
                        .unwrap()
                        .state
                )
                .unwrap(),
                expected_change,
            );
        }
    }
}
