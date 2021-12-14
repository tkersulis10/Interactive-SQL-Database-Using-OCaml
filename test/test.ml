open OUnit2
open Database
open Main

(* TEST PLAN: Our test suite tested the many different functions inside
   of src/main.ml. These are the functions that dictate the
   functionality of the database. They perform different SQL operations
   on the database such as adding, updating, and finding values as well
   as creating databases and other key operations. All of these
   functions in src/main.ml are tested in this test suite. We did this
   by constructing multiple test json files that have an existing
   database in each one. Each test json file has a block of tests
   associated with it. These blocks of tests are split up in the test
   suite below and are labeled first_section_tests,
   second_section_tests, third_section_tests, fourth_section_tests,
   computation_tests, and exception_tests. Each section of tests
   correspond to a different json file that we are testing, except for
   exception_tests which we will explain later.

   In the first four blocks of tests (first_section_tests,
   second_section_tests, third_section_tests, and fourth_section_tests)
   we performed a number of functions that change the json file
   corresponding to that json file. For example, these functions may add
   a new row to the database, update the database, or sort the values in
   the database. We then go through and test the functions in
   src/main.ml that read a database and output a value corresponding to
   that database. We go through every function in src/main.ml that
   returns a value and test that the value matches our expected output.
   This proves to us not only that these functions that output a value
   are working correctly, but also that the functions that we called at
   the beginning of the block of tests produced the correct new
   database, and therefore are also working correctly. In these four
   blocks of tests we made sure that each function that causes a change
   in the database to be called to prove that each function works
   properly.

   In the fifth block of tests (computation_tests), we used a
   pre-determined test json file named computatoin_database.json. This
   database json file is a database that has many boundary conditions
   and other values in it that we were not able to test in the first
   four blocks of tests. Therefore, we did not call any additional
   functions on it that change the database beforehand this time. We
   simply want to test our output functions for these certain values to
   prove to us that these functions still work on less common values.

   In the sixth block of tests (exception_tests), we called functions
   that should produce exceptions when called to make sure the
   exceptions in our code worked properly. We used assert_raises to
   ensure that the right exception is raised and we used multiple of the
   test json files that we used in the previous blocks of tests to make
   sure that exceptions worked on all of the json files. We also made
   sure that the exception tests would not alter any of the databases
   and instead only raise exceptions. This proved to us that our system
   could handle bad inputs.

   Our OUnit test cases cover the database functionality functions
   through primarily black box testing. The test cases were developed by
   looking at the specifications of the different functions and making
   sure that they would work correctly for not only good inputs, but
   also through bad inputs as discussed during our exception_tests. We
   created test cases by going through the specification and entering
   typical and boundary inputs for the arguments. Our command line in
   CLI/main.ml, on the other hand, was manually tested. Since we already
   tested the database functions used by the command line using OUnit,
   we were confident of the correctness of those functions in the
   command line. Therefore, we manually tested how the command line
   interacted with different syntax errors, good inputs by users, and
   bad inputs by users by running make cli and then testing inputs into
   the command line. We also went through each command and made sure
   that the syntax worked properly and that the interface was clear and
   worked as intended.

   As stated above, our testing approach demonstrates the correctness of
   the system because it goes through all of the functions and checks
   that each side effect to the database and each output from a read
   from the database matches. Finally, after we run make test to run the
   test cases in OUnit, we must reset the test json files to how they
   were before running the test file since the test cases alter the test
   json files. This ensures that the tests are consistent and work every
   time we want to run make test. *)

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

(** [add_field_to_all_rows_test name file database_name field_name field_type expected_output]
    creates an OUnit test with name [name] that compares whether
    [file |> dbs_from_file |> Yojson.Basic.Util.to_string] is equal to
    [expected_output] after calling
    [add_field_to_all_rows file database_name field_name field_type]. *)
let add_field_to_all_rows_test
    (name : string)
    (file : string)
    (database_name : string)
    (field_name : string)
    (field_type : string)
    (expected_output : string) =
  let _ =
    add_field_to_all_rows file database_name field_name field_type
  in
  name >:: fun _ ->
  assert_equal expected_output
    (file |> dbs_from_file |> Yojson.Basic.to_string)
    ~printer:identity

(** [string_of_int_list lst] converts the int list [lst] to a string
    representation. *)
let rec string_of_int_list (lst : int list) : string =
  match lst with
  | [] -> ""
  | h :: t -> string_of_int h ^ ", " ^ string_of_int_list t

(** [find_row_test name file database_name field_name value_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [find_row file database_name field_name value_name] is equal to
    [expected_output]. *)
let find_row_test
    (name : string)
    (file : string)
    (database_name : string)
    (field_name : string)
    (value_name : string)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (find_row file database_name field_name value_name)
    ~printer:string_of_int_list

(** [get_fields_list_test name file database_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [get_fields_list file database_name] is equal to [expected_output]. *)
let get_fields_list_test
    (name : string)
    (file : string)
    (database_name : string)
    (expected_output : string list) =
  name >:: fun _ ->
  assert_equal expected_output
    (get_fields_list file database_name)
    ~printer:(String.concat ", ")

(** [sum_of_field_test name file database_name field_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [sum_of_field file database_name field_name] is equal to
    [expected_output]. *)
let sum_of_field_test
    (name : string)
    (file : string)
    (database_name : string)
    (field_name : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (sum_of_field file database_name field_name)
    ~printer:identity

(** [mean_of_field_test name file database_name field_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [mean_of_field file database_name field_name] is equal to
    [expected_output]. *)
let mean_of_field_test
    (name : string)
    (file : string)
    (database_name : string)
    (field_name : string)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (mean_of_field file database_name field_name)
    ~printer:identity

(** [update_file_test name file expected_output] creates an OUnit test
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
  let _ = delete_database "empty_database.json" "test" in
  let _ =
    add_database "empty_database.json" "test"
      [
        ("1", "string");
        ("2", "string");
        ("3", "string");
        ("4", "string");
        ("5", "string");
      ]
  in
  let _ =
    add_database "empty_database.json" "Cornell"
      [
        ("Engineering", "string"); ("CALS", "string"); ("A&S", "string");
      ]
  in
  let _ =
    add_database "empty_database.json" "Users"
      [ ("Tomas", "string"); ("Max", "string") ]
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
      "{\"test\":[{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"}],\"Cornell\":[{\"Engineering\":\"string\",\"CALS\":\"string\",\"A&S\":\"string\",\"Hotel\":\"\"}],\"Users\":[{\"Tomas\":\"string\",\"Max\":\"string\"}]}";
    database_string_test "database_string for empty_database.json"
      "empty_database.json"
      "{\"test\":[{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"}],\"Cornell\":[{\"Engineering\":\"string\",\"CALS\":\"string\",\"A&S\":\"string\",\"Hotel\":\"\"}],\"Users\":[{\"Tomas\":\"string\",\"Max\":\"string\"}]}";
    splice_outer_parens_test
      "splice_outer_parens_test for empty_database.json"
      "empty_database.json"
      "\"test\":[{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"}],\"Cornell\":[{\"Engineering\":\"string\",\"CALS\":\"string\",\"A&S\":\"string\",\"Hotel\":\"\"}],\"Users\":[{\"Tomas\":\"string\",\"Max\":\"string\"}],";
    find_database_test "find_database for empty_database.json"
      "empty_database.json" "Cornell"
      "{\"Engineering\":\"string\",\"CALS\":\"string\",\"A&S\":\"string\",\"Hotel\":\"\"}";
    find_database_test "find_database for empty_database.json"
      "empty_database.json" "Users"
      "{\"Tomas\":\"string\",\"Max\":\"string\"}";
    find_value_in_database_test
      "find_value_in_database for empty_database.json"
      "empty_database.json" "Users" "Tomas"
      "\n  - No results found. Database is empty.";
    get_db_names_list_test "get_db_names for empty_database.json"
      "empty_database.json" "  -  test\n  -  Cornell\n  -  Users\n\n";
    get_fields_list_test "get_fields_list for empty_database.json"
      "empty_database.json" "test"
      [ "1"; "2"; "3"; "4"; "5" ];
    get_fields_list_test "get_fields_list for empty_database.json"
      "empty_database.json" "Cornell"
      [ "Engineering"; "CALS"; "A&S"; "Hotel" ];
  ]

let second_section_tests =
  let _ =
    update_all "test_database2.json" "Users" "fact" "CS 3110 is fun."
  in
  let _ = update_value "test_database2.json" "test" "2" 1 "zero, 0" in
  let _ =
    add_field_to_all_rows "test_database2.json" "Users" "Idea" "string"
  in
  [
    update_file_test
      "tests update_all, update_element, and add_field_to_all_rows in \
       test_database2.json"
      "test_database2.json"
      "{\"Users\":[{\"name\":\"string\",\"age\":\"string\",\"fact\":\"string\",\"Idea\":\"string\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"},{\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"}],\"test\":[{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"},{\"1\":\"a\",\"2\":\"zero, \
       0\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}]}";
    database_string_test "database_string for test_database2.json"
      "test_database2.json"
      "{\"Users\":[{\"name\":\"string\",\"age\":\"string\",\"fact\":\"string\",\"Idea\":\"string\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"},{\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"}],\"test\":[{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"},{\"1\":\"a\",\"2\":\"zero, \
       0\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}]}";
    splice_outer_parens_test
      "splice_outer_parens_test for test_database2.json"
      "test_database2.json"
      "\"Users\":[{\"name\":\"string\",\"age\":\"string\",\"fact\":\"string\",\"Idea\":\"string\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"},{\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"CS \
       3110 is \
       fun.\",\"Idea\":\"\"}],\"test\":[{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"},{\"1\":\"a\",\"2\":\"zero, \
       0\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}],";
    find_database_test "find_database for test_database2.json"
      "test_database2.json" "Users"
      "{\"name\":\"string\",\"age\":\"string\",\"fact\":\"string\",\"Idea\":\"string\"}, \
       {\"name\":\"Max\",\"age\":\"20\",\"fact\":\"CS 3110 is \
       fun.\",\"Idea\":\"\"}, \
       {\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"CS 3110 is \
       fun.\",\"Idea\":\"\"}";
    find_database_test "find_database for test_database2.json"
      "test_database2.json" "test"
      "{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"}, \
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
    find_row_test
      "find_row for test_database2.json where there are multiple \
       occurences of a value"
      "test_database2.json" "Users" "Idea" "" [ 1; 2 ];
    find_row_test
      "find_row for test_database2.json where there is one occurrence \
       of a value"
      "test_database2.json" "test" "2" "zero, 0" [ 1 ];
    find_row_test
      "find_row for test_database2.json where there is one occurrence \
       of a value"
      "test_database2.json" "Users" "age" "infinite" [ 2 ];
    get_fields_list_test "get_fields_list for test_database2.json"
      "test_database2.json" "Users"
      [ "name"; "age"; "fact"; "Idea" ];
  ]

let third_section_tests =
  let _ = sort_rows "test_database3.json" "Users" "fact" in
  let _ = sort_rows "test_database3.json" "test" "3" in
  let _ = sort_rows "test_database3.json" "Users" "age" in
  let _ = sort_rows "test_database3.json" "test" "2" in
  [
    update_file_test
      "tests sort_field_string, sort_field_int, and sort_field_general \
       in test_database3.json"
      "test_database3.json"
      "{\"Users\":[{\"name\":\"string\",\"age\":\"string\",\"fact\":\"string\"},{\"name\":\"cli\",\"age\":\"1\",\"fact\":\"is \
       computer\"},{\"name\":\"Max\",\"age\":\"20\",\"fact\":\"is \
       writing code right \
       now.\"},{\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"is \
       the bringer of \
       chaos.\"}],\"test\":[{\"1\":\"string\",\"2\":\"int\",\"3\":\"int\",\"tvalue\":\"string\",\"opval\":\"string\"},{\"1\":\"1 \
       1\",\"2\":\"-1239999\",\"3\":\"-43\",\"tvalue\":\"\",\"opval\":\"yay\"},{\"1\":\"l \
       o \
       l\",\"2\":\"-23423\",\"3\":\"5\",\"tvalue\":\"\",\"opval\":\"nay\"},{\"1\":\"l \
       o \
       l\",\"2\":\"0\",\"3\":\"3\",\"tvalue\":\"\",\"opval\":\"nay\"},{\"1\":\"l \
       o \
       l\",\"2\":\"73\",\"3\":\"33\",\"tvalue\":\"\",\"opval\":\"nay\"}]}";
    find_database_test "find_database for test_database3.json"
      "test_database3.json" "Users"
      "{\"name\":\"string\",\"age\":\"string\",\"fact\":\"string\"}, \
       {\"name\":\"cli\",\"age\":\"1\",\"fact\":\"is computer\"}, \
       {\"name\":\"Max\",\"age\":\"20\",\"fact\":\"is writing code \
       right now.\"}, \
       {\"name\":\"Cthulu\",\"age\":\"infinite\",\"fact\":\"is the \
       bringer of chaos.\"}";
    find_database_test "find_database for test_database3.json"
      "test_database3.json" "test"
      "{\"1\":\"string\",\"2\":\"int\",\"3\":\"int\",\"tvalue\":\"string\",\"opval\":\"string\"}, \
       {\"1\":\"1 \
       1\",\"2\":\"-1239999\",\"3\":\"-43\",\"tvalue\":\"\",\"opval\":\"yay\"}, \
       {\"1\":\"l o \
       l\",\"2\":\"-23423\",\"3\":\"5\",\"tvalue\":\"\",\"opval\":\"nay\"}, \
       {\"1\":\"l o \
       l\",\"2\":\"0\",\"3\":\"3\",\"tvalue\":\"\",\"opval\":\"nay\"}, \
       {\"1\":\"l o \
       l\",\"2\":\"73\",\"3\":\"33\",\"tvalue\":\"\",\"opval\":\"nay\"}";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "Users" "name"
      "\n\"cli\", \"Max\", \"Cthulu\"";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "Users" "age"
      "\n\"1\", \"20\", \"infinite\"";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "Users" "fact"
      "\n\
       \"is computer\", \"is writing code right now.\", \"is the \
       bringer of chaos.\"";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "test" "2"
      "\n\"-1239999\", \"-23423\", \"0\", \"73\"";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "test" "3" "\n\"-43\", \"5\", \"3\", \"33\"";
    get_db_names_list_test "get_db_names for test_database3.json"
      "test_database3.json" "  -  Users\n  -  test\n\n";
    find_row_test
      "find_row for test_database3.json where there are multiple \
       occurences of a value"
      "test_database3.json" "test" "opval" "nay" [ 2; 3; 4 ];
    find_row_test
      "find_row for test_database3.json where there is one occurrence \
       of a value"
      "test_database3.json" "test" "opval" "yay" [ 1 ];
    find_row_test
      "find_row for test_database3.json where there is one occurrence \
       of a value"
      "test_database3.json" "Users" "fact" "is writing code right now."
      [ 2 ];
    get_fields_list_test
      "get_fields_list for Users in test_database3.json"
      "test_database3.json" "Users"
      [ "name"; "age"; "fact" ];
    get_fields_list_test
      "get_fields_list for test in test_database3.json"
      "test_database3.json" "test"
      [ "1"; "2"; "3"; "tvalue"; "opval" ];
    sum_of_field_test
      "sum_of_field for 3 in test in test_database3.json"
      "test_database3.json" "test" "3" "-2";
    mean_of_field_test
      "mean_of_field for 3 in test in test_database3.json"
      "test_database3.json" "test" "3" "-0.5";
  ]

let fourth_section_tests =
  let _ =
    add_row "test_database.json" "Users" [ "hello"; "-50"; "bye" ]
  in
  let _ =
    add_row "test_database.json" "test" [ "f"; "g"; "h"; "i"; "j" ]
  in
  let _ =
    add_row "test_database.json" "test" [ "k"; "l"; "m"; "n"; "o" ]
  in
  let _ = delete_field "test_database.json" "Users" "name" in
  let _ = sort_rows "test_database.json" "Users" "age" in
  [
    update_file_test
      "tests add_row, delete_field, and sort_rows in test_database.json"
      "test_database.json"
      "{\"Users\":[{\"age\":\"int\",\"fact\":\"string\"},{\"age\":\"-50\",\"fact\":\"bye\"},{\"age\":\"20\",\"fact\":\"is \
       writing code right \
       now.\"}],\"test\":[{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"},{\"1\":\"a\",\"2\":\"b\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"},{\"1\":\"f\",\"2\":\"g\",\"3\":\"h\",\"4\":\"i\",\"5\":\"j\"},{\"1\":\"k\",\"2\":\"l\",\"3\":\"m\",\"4\":\"n\",\"5\":\"o\"}]}";
    find_database_test "find_database for test_database.json"
      "test_database.json" "Users"
      "{\"age\":\"int\",\"fact\":\"string\"}, \
       {\"age\":\"-50\",\"fact\":\"bye\"}, \
       {\"age\":\"20\",\"fact\":\"is writing code right now.\"}";
    find_database_test "find_database for test_database.json"
      "test_database.json" "test"
      "{\"1\":\"string\",\"2\":\"string\",\"3\":\"string\",\"4\":\"string\",\"5\":\"string\"}, \
       {\"1\":\"a\",\"2\":\"b\",\"3\":\"c\",\"4\":\"d\",\"5\":\"e\"}, \
       {\"1\":\"f\",\"2\":\"g\",\"3\":\"h\",\"4\":\"i\",\"5\":\"j\"}, \
       {\"1\":\"k\",\"2\":\"l\",\"3\":\"m\",\"4\":\"n\",\"5\":\"o\"}";
    find_value_in_database_test
      "find_value_in_database for test_database.json"
      "test_database.json" "Users" "age" "\n\"-50\", \"20\"";
    find_value_in_database_test
      "find_value_in_database for test_database.json"
      "test_database.json" "Users" "fact"
      "\n\"bye\", \"is writing code right now.\"";
    find_value_in_database_test
      "find_value_in_database for test_database.json"
      "test_database.json" "test" "1" "\n\"a\", \"f\", \"k\"";
    find_value_in_database_test
      "find_value_in_database for test_database.json"
      "test_database.json" "test" "2" "\n\"b\", \"g\", \"l\"";
    find_value_in_database_test
      "find_value_in_database for test_database.json"
      "test_database.json" "test" "3" "\n\"c\", \"h\", \"m\"";
    get_db_names_list_test "get_db_names for test_database.json"
      "test_database.json" "  -  Users\n  -  test\n\n";
    find_row_test
      "find_row for test_database.json where there is one occurrence \
       of a value"
      "test_database.json" "test" "3" "m" [ 3 ];
    find_row_test
      "find_row for test_database.json where there is one occurrence \
       of a value"
      "test_database.json" "Users" "age" "20" [ 2 ];
    get_fields_list_test
      "get_fields_list for Users in test_database.json"
      "test_database.json" "Users" [ "age"; "fact" ];
    get_fields_list_test
      "get_fields_list for test in test_database.json"
      "test_database.json" "test"
      [ "1"; "2"; "3"; "4"; "5" ];
    sum_of_field_test "sum_of_field for 3 in test in test_database.json"
      "test_database.json" "test" "3" "\"c, h, m\"";
    sum_of_field_test
      "sum_of_field for fact in Users in test_database.json"
      "test_database.json" "Users" "fact"
      "\"bye, is writing code right now.\"";
    sum_of_field_test
      "sum_of_field for age in Users in test_database.json"
      "test_database.json" "Users" "age" "-30";
    mean_of_field_test
      "mean_of_field for age in Users in test_database.json"
      "test_database.json" "Users" "age" "-15.";
  ]

let computation_tests =
  [
    sum_of_field_test "sum of int values in computation_database.json"
      "computation_database.json" "Types" "int" "-47";
    mean_of_field_test "mean of int values in computation_database.json"
      "computation_database.json" "Types" "int" "-15.6666666667";
    sum_of_field_test "sum of float values in computation_database.json"
      "computation_database.json" "Types" "float" "39.3";
    mean_of_field_test
      "mean of float values in computation_database.json"
      "computation_database.json" "Types" "float" "13.1";
    sum_of_field_test
      "sum of string values in computation_database.json"
      "computation_database.json" "Types" "string" "\"cs3110,  , \\n\"";
    sum_of_field_test
      "sum of string values in computation_database.json"
      "computation_database.json" "Mismatch" "1"
      "\"50.5, 0, true, false\"";
    sum_of_field_test "sum of bool values in computation_database.json"
      "computation_database.json" "Types" "bool" "true";
    sum_of_field_test
      "sum of string values in computation_database.json"
      "computation_database.json" "Mismatch" "Weird Notation"
      "\"\\\", '', yes,  \"";
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
    ( "find_row for empty_database.json where the value name is not in \
       the database"
    >:: fun _ ->
      assert_raises (ValNotFound "Clarkson") (fun () ->
          find_row "empty_database.json" "Cornell" "Engineering"
            "Clarkson") );
    ( "find_row for test_database2.json where the field name is not in \
       the database"
    >:: fun _ ->
      assert_raises (FieldNotFound "hello") (fun () ->
          find_row "test_database2.json" "Users" "hello" "20") );
    ( "get_fields_list for empty_database.json where the database is \
       not in the file"
    >:: fun _ ->
      assert_raises (DatabaseNotFound "Test") (fun () ->
          get_fields_list "empty_database.json" "Test") );
    ( "sort_field_string for test_database3.json where database is not \
       file"
    >:: fun _ ->
      assert_raises (DatabaseNotFound "users") (fun () ->
          sort_rows "test_database3.json" "users" "age") );
    ( "sort_field_int for test_database3.json where field is not file"
    >:: fun _ ->
      assert_raises (FieldNotFound "4") (fun () ->
          sort_rows "test_database3.json" "test" "4") );
    ( "mean_of_field for computation_database.json where field is not \
       int or float"
    >:: fun _ ->
      assert_raises
        (WrongType ("", "bool"))
        (fun () ->
          mean_of_field "computation_database.json" "Types" "bool") );
    ( "mean_of_field for computation_database.json where field is not \
       int or float"
    >:: fun _ ->
      assert_raises
        (WrongType ("", "string"))
        (fun () ->
          mean_of_field "computation_database.json" "Types" "string") );
    ( "sum_of_field for computation_database.json where field is float \
       instead of int"
    >:: fun _ ->
      assert_raises
        (WrongType ("", "int"))
        (fun () ->
          sum_of_field "computation_database.json" "Mismatch"
            "Int mismatch") );
    ( "mean_of_field for computation_database.json where field is \
       string instead of int"
    >:: fun _ ->
      assert_raises
        (WrongType ("", "int"))
        (fun () ->
          mean_of_field "computation_database.json" "Mismatch" "Max/Min")
    );
    ( "mean_of_field for test_database.json where field is string \
       instead of int"
    >:: fun _ ->
      assert_raises
        (WrongType ("", "string"))
        (fun () -> mean_of_field "test_database.json" "test" "3") );
    ( "add_row for test_database.json where number of values does not \
       match the database (no values given)"
    >:: fun _ ->
      assert_raises InvalidShape (fun () ->
          add_row "test_database.json" "Users" []) );
    ( "add_row for test_database.json where number of values does not \
       match the database (more values than needed are given)"
    >:: fun _ ->
      assert_raises InvalidShape (fun () ->
          add_row "test_database.json" "test"
            [ "a"; "b"; "c"; "d"; "e"; "f" ]) );
    ( "add_row for test_database3.json where number of values does not \
       match the database (less values than needed are given)"
    >:: fun _ ->
      assert_raises InvalidShape (fun () ->
          add_row "test_database3.json" "test" [ "5"; "-5" ]) );
    ( "add_row for test_database.json where added value does not match \
       the field type in field age of Users"
    >:: fun _ ->
      assert_raises
        (WrongType ("hello", "int"))
        (fun () ->
          add_row "test_database.json" "Users" [ "hello"; "bye" ]) );
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           first_section_tests;
           second_section_tests;
           third_section_tests;
           fourth_section_tests;
           computation_tests;
           exception_tests;
         ]

let _ = run_test_tt_main suite
