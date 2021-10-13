open OUnit2
open Database
open Main

let main_tests = []

let suite =
  "test suite for final project" >::: List.flatten [ main_tests ]

let _ = run_test_tt_main suite
