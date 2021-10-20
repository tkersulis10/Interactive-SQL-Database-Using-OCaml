open OUnit2
open Database
open Main

let test_database_1 = dbs_from_file "database.json"

let test_database_2 = dbs_from_file "test_database.json"

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
  assert_equal expected_output (database_string input) ~printer:identity

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
    (expected_output : string) =
  let _ = add_database file database_name values in
  name >:: fun _ ->
  assert_equal expected_output (database_string file) ~printer:identity

(** [delete_database_test name file database_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling
    [delete_database file database_name]. *)
let delete_database_test
    (name : string)
    (file : string)
    (database_name : string)
    (expected_output : string) =
  let _ = delete_database file database_name in
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.Util.to_string)
    ~printer:identity

(** [clear_database_file_test name input expected_output] creates an
    OUnit test with name [name] that compares whether
    [input |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling [clear_database_file input]. *)
let clear_database_file_test
    (name : string)
    (input : string)
    (expected_output : string) =
  let _ = clear_database_file input in
  name >:: fun _ ->
  assert_equal expected_output
    (input |> dbs_from_file |> Yojson.Basic.Util.to_string)
    ~printer:identity

(** [clear_database_test name file database_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling [clear_database file database_name]. *)
let clear_database_test
    (name : string)
    (file : string)
    (database_name : string)
    (expected_output : string) =
  let _ = clear_database file database_name in
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.Util.to_string)
    ~printer:identity

(** [find_database_test name file database_name expected_output] creates
    an OUnit test with name [name] that compares whether
    [find_database file database_name] is equal to [expected_output]. *)
let find_database_test
    (name : string)
    (file : string)
    (database_name : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (Yojson.Basic.Util.to_string (find_database file database_name))
    ~printer:identity

(** [find_database_test name file database_name expected_output] creates
    an OUnit test with name [name] that compares whether
    [find_database file database_name] is equal to [expected_output]. *)
let find_value_in_database_test
    (name : string)
    (file : string)
    (database_name : string)
    (value_name : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (find_value_in_database file database_name value_name)
    ~printer:identity

let main_tests =
  [
    database_list_test "database_list for database.json" "database.json"
      "{\"testDB2\":{\"t1\":\"\",\"t2\":\"\",\"t3\":\"\"}}";
    write_to_file_test "write_to_file for database.json"
      "test_database.json" test_database_1 test_database_1;
    splice_outer_parens_test "splice_outer_parens for database.json"
      (database_string "database.json")
      "\"testDB2\":{\"t1\":\"\",\"t2\":\"\",\"t3\":\"\"},";
    add_database_test "add_database for empty_database.json"
      "empty_database.json" "test_database" [ "hi"; "bye" ]
      "\"test_database\":{\"hi\":\"\",\"bye\":\"\"}";
    delete_database_test
      "delete_database for database.json where name does not match a \
       database"
      "database.json" "Hello"
      "{\"testDB2\":{\"t1\":\"\",\"t2\":\"\",\"t3\":\"\"}}";
    delete_database_test
      "delete_database for database.json where name does match a \
       database"
      "database.json" "testDB2" "{}";
    clear_database_file_test
      "clear_database_file_test for empty_database.json"
      "empty_database.json" "{}";
    clear_database_test
      "clear_database for test_database.json where name does not match \
       a database"
      "test_database.json" "CS3110"
      (Yojson.Basic.to_string test_database_2);
    (let _ =
       add_database "test_database.json" "testDB4" [ "1"; "2"; "3" ]
     in
     clear_database_test
       "clear_database for test_database.json where name does match a \
        database"
       "test_database.json" "testDB4" "{\"testDB2\":[{}]}");
    (let _ = add_database "database.json" "testDB3" [ "1"; "2"; "3" ] in
     find_database_test
       "find_database for database.json when the database does exist \
        in file"
       "database.json" "testDB3" "{\"1\":\"\",\"1\":\"\",\"3\":\"\"}");
    ( "find_database for database.json when the database does not \
       exist in the file"
    >:: fun _ ->
      assert_raises (DatabaseNotFound "testDB78") (fun () ->
          find_database "database.json" "testDB78") );
    find_value_in_database_test
      "find_value_in_database for database.json when value does exist \
       in database"
      "database.json" "testDB3" "2" "\"\"";
    ( "find_value_in_database for database.json when the value does \
       not exist in the database"
    >:: fun _ ->
      assert_raises (ValNotFound "4") (fun () ->
          find_value_in_database "database.json" "testDB3" "4") );
    ( "find_value_in_database for database.json when the database does \
       not exist in the file, but the value does exist in the file"
    >:: fun _ ->
      assert_raises (ValNotFound "3") (fun () ->
          find_value_in_database "database.json" "testDB0" "3") );
  ]

let suite =
  "test suite for final project" >::: List.flatten [ main_tests ]

let _ = run_test_tt_main suite
