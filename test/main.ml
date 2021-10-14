open OUnit2
open Database
open Main

let test_database_1 = dbs_from_file "database.json"

(** [identity s] outputs the same string string [s] that is input. *)
let identity (s : string) : string = s

(** [database_list_test name input expected_output] creates an OUnit
    test with name [name] and ensures that [expected_output] is equal to
    [database_list input]. *)
let database_list_test
    (name : string)
    (input : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (database_list input) ~printer:identity

(** [write_to_file_test name file db expected_output] creates an OUnit
    test with name [name] that compares whether [write_to_file file db]
    writes the same database to [file] as [expected_output]. *)
let write_to_file_test
    (name : string)
    (file : string)
    (db : Yojson.Basic.t)
    expected_output =
  let _ = write_to_file file db in
  name >:: fun _ ->
  assert_equal expected_output (dbs_from_file file)
    ~printer:Yojson.Basic.to_string

(** [splice_outer_parens_test name input expected_output] creates an
    OUnit test with name [name] and ensures that [expected_output] is
    equal to [splice_outer_parens input]. *)
let splice_outer_parens_test
    (name : string)
    (input : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (splice_outer_parens input)
    ~printer:identity

(** [add_database_test name file database_name values expected_output]
    creates an OUnit test with name [name] that compares whether
    [add_database file database_name values] writes the same database to
    [file] as [expected_output]. *)
let add_database_test
    (name : string)
    (file : string)
    (database_name : string)
    (values : string list)
    expected_output =
  let _ = add_database file database_name values in
  name >:: fun _ ->
  assert_equal expected_output (database_list file) ~printer:identity

let main_tests =
  [
    database_list_test "database_list for database.json" "database.json"
      "{\"testDB2\":{\"t1\":\"\",\"t2\":\"\",\"t3\":\"\"}}";
    write_to_file_test "write_to_file for database.json"
      "test_database.json" test_database_1 test_database_1;
    splice_outer_parens_test "splice_outer_parens for database.json"
      (database_list "database.json")
      "\"testDB2\":{\"t1\":\"\",\"t2\":\"\",\"t3\":\"\"},";
    add_database_test "add_database for database.json"
      "empty_database.json" "test_database" [ "hi"; "bye" ]
      "\"test_database\":{\"hi\":\"\",\"bye\":\"\"}";
  ]

let suite =
  "test suite for final project" >::: List.flatten [ main_tests ]

let _ = run_test_tt_main suite
