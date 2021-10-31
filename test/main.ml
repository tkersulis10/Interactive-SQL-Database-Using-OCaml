open OUnit2
open Database
open Main

let test_database_helper = dbs_from_file "test_helper_database.json"

let test_database_1 = dbs_from_file "test_database.json"

let test_database_2 = dbs_from_file "test_database2.json"

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
    (file |> dbs_from_file |> Yojson.Basic.to_string)
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
    (input |> dbs_from_file |> Yojson.Basic.to_string)
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
    (file |> dbs_from_file |> Yojson.Basic.to_string)
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
    (find_database file database_name
    |> Yojson.Basic.Util.to_list
    |> List.map (fun x -> Yojson.Basic.to_string x)
    |> String.concat ", ")
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

(** [get_db_names_list_test name input expected_output] creates an OUnit
    test with name [name] that compares whether
    [get_db_names_list input] is equal to [expected_output]. *)
let get_db_names_list_test
    (name : string)
    (input : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (get_db_names_list input)
    ~printer:identity

(** [list_rows_test name input expected_output] creates an OUnit test
    with name [name] that compares whether
    [list_rows file database_name] is equal to [expected_output]. *)
let list_rows_test
    (name : string)
    (file : string)
    (database_name : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (list_rows file database_name)
    ~printer:identity

(** [add_element_to_database_test name file database_name value_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling
    [add_element_to_database file database_name value_name]. *)
let add_element_to_database_test
    (name : string)
    (file : string)
    (database_name : string)
    (value_name : string)
    (expected_output : string) =
  let _ = add_element_to_database file database_name value_name in
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.to_string)
    ~printer:identity

(** [add_element_to_all_database_test name file database_name value_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling
    [add_element_to_all_database file database_name value_name]. *)
let add_element_to_all_database_test
    (name : string)
    (file : string)
    (database_name : string)
    (value_name : string)
    (expected_output : string) =
  let _ = add_element_to_all_database file database_name value_name in
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.to_string)
    ~printer:identity

(** [update_element_test name file database_name value_name element_row new_value expected_output]
    creates an OUnit test with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling
    [update_element file database_name value_name element_row new_value]. *)
let update_element_test
    (name : string)
    (file : string)
    (database_name : string)
    (value_name : string)
    (element_row : int)
    (new_value : string)
    (expected_output : string) =
  let _ =
    update_element file database_name value_name element_row new_value
  in
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.to_string)
    ~printer:identity

(** [update_all_test name file database_name value_name new_value expected_output]
    creates an OUnit test with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling
    [update_all file database_name value_name new_value]. *)
let update_element_test
    (name : string)
    (file : string)
    (database_name : string)
    (value_name : string)
    (new_value : string)
    (expected_output : string) =
  let _ = update_all file database_name value_name new_value in
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.to_string)
    ~printer:identity

let main_tests =
  [
    database_list_test "database_list for test_database.json"
      "test_database.json"
      "{\"Users\":[{\"name\":\"\",\"age\":\"\",\"fact\":\"\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"is \
       writing code right \
       now.\"}],\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"},{\"1\":\"a\",\"2\":\"b\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}]}";
    write_to_file_test "write_to_file for test_database2.json"
      "test_database2.json" test_database_helper test_database_helper;
    splice_outer_parens_test
      "splice_outer_parens for test_database.json"
      (database_string "test_database.json")
      "\"Users\":[{\"name\":\"\",\"age\":\"\",\"fact\":\"\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"is \
       writing code right \
       now.\"}],\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"},{\"1\":\"a\",\"2\":\"b\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}],";
    ( "delete_database for test_database.json where name does not \
       match a database"
    >:: fun _ ->
      assert_raises (DatabaseNotFound "Hello") (fun () ->
          delete_database "test_database.json" "Hello") );
    add_database_test "add_database for empty_database.json"
      "empty_database.json" "TEST" [ "hi"; "bye" ]
      "{\"TEST\":[{\"hi\":\"\",\"bye\":\"\"}]}";
    delete_database_test
      "delete_database for empty_database.json where name does match a \
       database, causing empty file"
      "test_database3.json" "Users" "{}";
    delete_database_test
      "delete_database for test_helper_database.json where name does \
       match a database, causing non-empty file (top database)"
      "test_helper_database.json" "Users"
      "{\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\"}]}";
    clear_database_file_test
      "clear_database_file_test for empty_database.json"
      "empty_database.json" "{}";
    clear_database_test
      "clear_database for test_database2.json where name does not \
       match a database"
      "test_database2.json" "CS3110"
      (Yojson.Basic.to_string test_database_helper);
    add_database_test "add testDB4 to empty_database.json"
      "empty_database.json" "testDB4" [ "1"; "2"; "3" ]
      "{\"testDB4\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\"}]}";
    clear_database_test
      "clear_database for empty_database.json where name does match a \
       database"
      "empty_database.json" "testDB4" "{\"testDB4\":[{}]}";
    find_database_test
      "find_database for test_database.json when the database does \
       exist in file"
      "test_database.json" "test"
      "{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"}, \
       {\"1\":\"a\",\"2\":\"b\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}";
    ( "find_database for empty_database.json when the database does \
       not exist in the file"
    >:: fun _ ->
      assert_raises (DatabaseNotFound "testDB78") (fun () ->
          find_database "empty_database.json" "testDB78") );
    find_value_in_database_test
      "find_value_in_database for test_database.json when value does \
       exist in database (bottom database)"
      "test_database.json" "test" "4" "\n\"d\"";
    find_value_in_database_test
      "find_value_in_database for test_database.json when value does \
       exist in database (top database)"
      "test_database.json" "Users" "name" "\n\"Max\"";
    ( "find_value_in_database for test_database.json when the value \
       does not exist in the database"
    >:: fun _ ->
      assert_raises (ValNotFound "5550") (fun () ->
          find_value_in_database "test_database.json" "Users" "5550") );
    ( "find_value_in_database for test_database.json when the database \
       does not exist in the file, but the value does exist in the \
       file"
    >:: fun _ ->
      assert_raises (DatabaseNotFound "testDB0") (fun () ->
          find_value_in_database "test_database.json" "testDB0" "20") );
  ]

let update_tests =
  [
    get_db_names_list_test "get_db_names_list for test_database.json"
      "test_database.json" "  -  Users\n  -  test\n\n";
    list_rows_test "list_rows for Users in test_database.json"
      "test_database.json" "Users"
      "(name: Max),  (age: 20),  (fact: is writing code right now.)\n\n";
    list_rows_test "list_rows for test in test_database.json"
      "test_database.json" "test"
      "(1: a),  (2: b),  (3: c),  (4: d),  (5: e)\n\n";
  ]

let suite =
  "test suite for final project"
  >::: List.flatten [ main_tests; update_tests ]

let _ = run_test_tt_main suite
