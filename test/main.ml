open OUnit2
open Database
open Main

let test_database_helper = dbs_from_file "test_helper_database.json"

let test_database_1 = dbs_from_file "test_database.json"

let test_database_2 = dbs_from_file "test_database2.json"

(** [identity s] outputs the same string string [s] that is input. *)
let identity (s : string) : string = s

(** [database_string_test name input expected_output] creates an OUnit
    test with name [name] and ensures that [expected_output] is equal to
    [database_list input]. *)
let database_string_test
    (name : string)
    (input : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (database_string input) ~printer:identity

(** [splice_outer_parens_test name input expected_output] creates an
    OUnit test with name [name] and ensures that [expected_output] is
    equal to [splice_outer_parens (database_string input)]. *)
let splice_outer_parens_test
    (name : string)
    (input : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (splice_outer_parens (database_string input))
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
    [find_value_in_database file database_name value_name] is equal to
    [expected_output]. *)
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

(** [list_rows_test name file database_name expected_output] creates an
    OUnit test with name [name] that compares whether
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

(** [add_field_to_all_rows_test name file database_name value_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling
    [add_field_to_all_rows file database_name value_name]. *)
let add_field_to_all_rows_test
    (name : string)
    (file : string)
    (database_name : string)
    (value_name : string)
    (expected_output : string) =
  let _ = add_field_to_all_rows file database_name value_name in
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.to_string)
    ~printer:identity

(** [update_file_test name file expected_output] creaes an OUnit test
    with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after a series of manipulations to [file]. *)
let update_file_test
    (name : string)
    (file : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.to_string)
    ~printer:identity

let first_section_tests =
  let _ = write_to_file "empty_database.json" test_database_1 in
  let _ = delete_database "empty_database.json" "Users" in
  let _ = clear_database "empty_database.json" "test" in
  let _ =
    add_database "empty_database.json" "Cornell"
      [ "Engineering"; "CALS"; "A&S" ]
  in
  let _ =
    add_database "empty_database.json" "Users" [ "Tomas"; "Max" ]
  in
  let _ =
    add_element_to_database "empty_database.json" "Cornell" "Hotel"
  in
  [
    update_file_test
      "tests write_to_file, delete_database, clear_database, \
       add_database, and add_element_to_database in  \
       empty_database.json"
      "empty_database.json"
      "{\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"}],\"Cornell\":[{\"Engineering\":\"\",\"CALS\":\"\",\"A&S\":\"\",\"Hotel\":\"\"}],\"Users\":[{\"Tomas\":\"\",\"Max\":\"\"}]}";
    database_string_test "database_string for empty_database.json"
      "empty_database.json"
      "{\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"}],\"Cornell\":[{\"Engineering\":\"\",\"CALS\":\"\",\"A&S\":\"\",\"Hotel\":\"\"}],\"Users\":[{\"Tomas\":\"\",\"Max\":\"\"}]}";
    splice_outer_parens_test
      "splice_outer_parens_test for empty_database.json"
      "empty_database.json"
      "\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"}],\"Cornell\":[{\"Engineering\":\"\",\"CALS\":\"\",\"A&S\":\"\",\"Hotel\":\"\"}],\"Users\":[{\"Tomas\":\"\",\"Max\":\"\"}],";
    find_database_test "find_database for empty_database.json"
      "empty_database.json" "Cornell"
      "{\"Engineering\":\"\",\"CALS\":\"\",\"A&S\":\"\",\"Hotel\":\"\"}";
    find_database_test "find_database for empty_database.json"
      "empty_database.json" "Users" "{\"Tomas\":\"\",\"Max\":\"\"}";
    find_value_in_database_test
      "find_value_in_database for empty_database.json"
      "empty_database.json" "Users" "Tomas"
      "\n  - No results found. Database is empty.";
    get_db_names_list_test "get_db_names for empty_database.json"
      "empty_database.json" "  -  test\n  -  Cornell\n  -  Users\n\n";
    list_rows_test "list_rows for empty_database.json"
      "empty_database.json" "Cornell" "\n - Database is empty.\n\n";
  ]

let second_section_tests =
  let _ =
    update_all "test_database2.json" "Users" "fact" "CS 3110 is fun."
  in
  let _ = update_value "test_database2.json" "test" "2" 1 "zero, 0" in
  let _ = add_field_to_all_rows "test_database2.json" "Users" "Idea" in
  [
    update_file_test
      "tests update_all, update_element, and add_field_to_all_rows in \
       test_database2.json"
      "test_database2.json"
      "{\"Users\":[{\"name\":\"\",\"age\":\"\",\"fact\":\"\",\"Idea\":\"\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"},{\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"}],\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"},{\"1\":\"a\",\"2\":\"zero, \
       0\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}]}";
    database_string_test "database_string for test_database2.json"
      "test_database2.json"
      "{\"Users\":[{\"name\":\"\",\"age\":\"\",\"fact\":\"\",\"Idea\":\"\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"},{\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"}],\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"},{\"1\":\"a\",\"2\":\"zero, \
       0\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}]}";
    splice_outer_parens_test
      "splice_outer_parens_test for test_database2.json"
      "test_database2.json"
      "\"Users\":[{\"name\":\"\",\"age\":\"\",\"fact\":\"\",\"Idea\":\"\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"},{\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"}],\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"},{\"1\":\"a\",\"2\":\"zero, \
       0\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}],";
    find_database_test "find_database for test_database2.json"
      "test_database2.json" "Users"
      "{\"name\":\"\",\"age\":\"\",\"fact\":\"\",\"Idea\":\"\"}, \
       {\"name\":\"Max\",\"age\":\"20\",\"fact\":\"CS 3110 is \
       fun.\",\"Idea\":\"\"}, \
       {\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"CS 3110 is \
       fun.\",\"Idea\":\"\"}";
    find_database_test "find_database for test_database2.json"
      "test_database2.json" "test"
      "{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"4\":\"\",\"5\":\"\"}, \
       {\"1\":\"a\",\"2\":\"zero, \
       0\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}";
    find_value_in_database_test
      "find_value_in_database for test_database2.json"
      "test_database2.json" "Users" "name" "\n\"Max\", \"Cthulu\"";
    find_value_in_database_test
      "find_value_in_database for test_database2.json"
      "test_database2.json" "test" "5" "\n\"e\"";
    get_db_names_list_test "get_db_names for test_database2.json"
      "test_database2.json" "  -  Users\n  -  test\n\n";
    list_rows_test "list_rows for test_database2.json"
      "test_database2.json" "Users"
      "(name: Max),  (age: 20),  (fact: CS 3110 is fun.),  (Idea: )\n\
       (name: Cthulu),  (age: infinite),  (fact: CS 3110 is fun.),  \
       (Idea: )\n\n";
    list_rows_test "list_rows for test_database2.json"
      "test_database2.json" "test"
      "(1: a),  (2: zero, 0),  (3: c),  (4: d),  (5: e)\n\n";
  ]

let exception_tests =
  [
    ( "delete_database for test_database.json where name does not \
       match a database"
    >:: fun _ ->
      assert_raises (DatabaseNotFound "Hello") (fun () ->
          delete_database "test_database.json" "Hello") );
    ( "find_database for empty_database.json when the database does \
       not exist in the file"
    >:: fun _ ->
      assert_raises (DatabaseNotFound "testDB78") (fun () ->
          find_database "empty_database.json" "testDB78") );
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
    ( "update_element for test_database.json where element_row = 0"
    >:: fun _ ->
      assert_raises (InvalidRow "Row cannot be less than 1") (fun () ->
          update_value "test_database.json" "test" "4" 0 "cs 3110") );
    ( "update_element for test_database.json where element_row < 0"
    >:: fun _ ->
      assert_raises (InvalidRow "Row cannot be less than 1") (fun () ->
          update_value "test_database.json" "Users" "age" ~-1 "cs 3110")
    );
    ( "update_element for test_database.json where element_row too large"
    >:: fun _ ->
      assert_raises (InvalidRow "Row not in database") (fun () ->
          update_value "test_database.json" "Users" "name" 5 "cs 3110")
    );
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [ first_section_tests; second_section_tests; exception_tests ]

let _ = run_test_tt_main suite
