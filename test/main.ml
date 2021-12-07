open OUnit2
open Database
open Main

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
    (expected_output : float) =
  name >:: fun _ ->
  assert_equal expected_output
    (sum_of_field file database_name field_name)
    ~printer:string_of_float

(** [mean_of_field_test name file database_name field_name expected_output]
    creates an OUnit test with name [name] that compares whether
    [mean_of_field file database_name field_name] is equal to
    [expected_output]. *)
let mean_of_field_test
    (name : string)
    (file : string)
    (database_name : string)
    (field_name : string)
    (expected_output : float) =
  name >:: fun _ ->
  assert_equal expected_output
    (mean_of_field file database_name field_name)
    ~printer:string_of_float

(** [computation_of_any_field_test name file database_name field_name element_of_string init computation expected_output]
    creates an OUnit test with name [name] that compares whether
    [computation_of_any_field file database_name field_name element_of_string init computation]
    is equal to [expected_output]. *)
let computation_of_any_field_test
    (name : string)
    (file : string)
    (database_name : string)
    (field_name : string)
    (element_of_string : string -> 'a)
    (init : 'a)
    (computation : 'a -> 'a -> 'a)
    (expected_output : 'a) =
  name >:: fun _ ->
  assert_equal expected_output
    (computation_of_any_field file database_name field_name
       element_of_string init computation)

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
    computation_of_any_field_test
      "computation_of_any_field with concatenation of strings with \
       field name fact in test_database2.json"
      "test_database2.json" "Users" "fact" identity "hello" ( ^ )
      "helloCS 3110 is fun.CS 3110 is fun.";
    computation_of_any_field_test
      "computation_of_any_field with only one string with that field \
       name in test_database2.json"
      "test_database2.json" "test" "2" identity "HI"
      (fun acc s -> acc ^ s ^ "BYE")
      "HIzero, 0BYE";
  ]

let third_section_tests =
  let _ =
    sort_field_string "test_database3.json" "Users" "fact" compare
  in
  let _ = sort_field_int "test_database3.json" "test" "3" compare in
  let _ =
    sort_field_general "test_database3.json" "Users" "age" identity
      identity compare
  in
  let _ =
    sort_field_general "test_database3.json" "test" "2" int_of_string
      string_of_int compare
  in
  [
    update_file_test
      "tests sort_field_string, sort_field_int, and sort_field_general \
       in test_database3.json"
      "test_database3.json"
      "{\"Users\":[{\"name\":\"\",\"age\":\"\",\"fact\":\"\"},{\"name\":\"Max\",\"age\":\"1\",\"fact\":\"is \
       computer\"},{\"name\":\"Cthulu\",\"age\":\"20\",\"fact\":\"is \
       the bringer of \
       chaos.\"},{\"name\":\"cli\",\"age\":\"infinite\",\"fact\":\"is \
       writing code right \
       now.\"}],\"test\":[{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"tvalue\":\"\",\"opval\":\"\"},{\"1\":\"l \
       o \
       l\",\"2\":\"-1239999\",\"3\":\"-43\",\"tvalue\":\"\",\"opval\":\"nay\"},{\"1\":\"l \
       o \
       l\",\"2\":\"-23423\",\"3\":\"3\",\"tvalue\":\"\",\"opval\":\"nay\"},{\"1\":\"l \
       o \
       l\",\"2\":\"0\",\"3\":\"5\",\"tvalue\":\"\",\"opval\":\"nay\"},{\"1\":\"1 \
       1\",\"2\":\"73\",\"3\":\"33\",\"tvalue\":\"\",\"opval\":\"yay\"}]}";
    find_database_test "find_database for test_database3.json"
      "test_database3.json" "Users"
      "{\"name\":\"\",\"age\":\"\",\"fact\":\"\"}, \
       {\"name\":\"Max\",\"age\":\"1\",\"fact\":\"is computer\"}, \
       {\"name\":\"Cthulu\",\"age\":\"20\",\"fact\":\"is the bringer \
       of chaos.\"}, \
       {\"name\":\"cli\",\"age\":\"infinite\",\"fact\":\"is writing \
       code right now.\"}";
    find_database_test "find_database for test_database3.json"
      "test_database3.json" "test"
      "{\"1\":\"\",\"2\":\"\",\"3\":\"\",\"tvalue\":\"\",\"opval\":\"\"}, \
       {\"1\":\"l o \
       l\",\"2\":\"-1239999\",\"3\":\"-43\",\"tvalue\":\"\",\"opval\":\"nay\"}, \
       {\"1\":\"l o \
       l\",\"2\":\"-23423\",\"3\":\"3\",\"tvalue\":\"\",\"opval\":\"nay\"}, \
       {\"1\":\"l o \
       l\",\"2\":\"0\",\"3\":\"5\",\"tvalue\":\"\",\"opval\":\"nay\"}, \
       {\"1\":\"1 \
       1\",\"2\":\"73\",\"3\":\"33\",\"tvalue\":\"\",\"opval\":\"yay\"}";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "Users" "name"
      "\n\"Max\", \"Cthulu\", \"cli\"";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "Users" "age"
      "\n\"1\", \"20\", \"infinite\"";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "Users" "fact"
      "\n\
       \"is computer\", \"is the bringer of chaos.\", \"is writing \
       code right now.\"";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "test" "2"
      "\n\"-1239999\", \"-23423\", \"0\", \"73\"";
    find_value_in_database_test
      "find_value_in_database for test_database3.json"
      "test_database3.json" "test" "3" "\n\"-43\", \"3\", \"5\", \"33\"";
    get_db_names_list_test "get_db_names for test_database3.json"
      "test_database3.json" "  -  Users\n  -  test\n\n";
    list_rows_test "list_rows for Users in test_database3.json"
      "test_database3.json" "Users"
      "(name: Max),  (age: 1),  (fact: is computer)\n\
       (name: Cthulu),  (age: 20),  (fact: is the bringer of chaos.)\n\
       (name: cli),  (age: infinite),  (fact: is writing code right \
       now.)\n\n";
    list_rows_test "list_rows for test in test_database3.json"
      "test_database3.json" "test"
      "(1: l o l),  (2: -1239999),  (3: -43),  (tvalue: ),  (opval: nay)\n\
       (1: l o l),  (2: -23423),  (3: 3),  (tvalue: ),  (opval: nay)\n\
       (1: l o l),  (2: 0),  (3: 5),  (tvalue: ),  (opval: nay)\n\
       (1: 1 1),  (2: 73),  (3: 33),  (tvalue: ),  (opval: yay)\n\n";
    find_row_test
      "find_row for test_database3.json where there are multiple \
       occurences of a value"
      "test_database3.json" "test" "opval" "nay" [ 1; 2; 3 ];
    find_row_test
      "find_row for test_database3.json where there is one occurrence \
       of a value"
      "test_database3.json" "test" "opval" "yay" [ 4 ];
    find_row_test
      "find_row for test_database3.json where there is one occurrence \
       of a value"
      "test_database3.json" "Users" "fact" "is writing code right now."
      [ 3 ];
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
      "test_database3.json" "test" "3" (-2.);
    mean_of_field_test
      "mean_of_field for 3 in test in test_database3.json"
      "test_database3.json" "test" "3" (-0.5);
    computation_of_any_field_test
      "computation_of_any_field for 2 in test in test_database3.json"
      "test_database3.json" "test" "2" int_of_string 0 ( + ) (-1263349);
  ]

let computation_tests =
  [
    sum_of_field_test "sum of int values in computation_database.json"
      "computation_database.json" "Types" "int" (-47.0);
    mean_of_field_test "mean of int values in computation_database.json"
      "computation_database.json" "Types" "int" (-15.6666666666666661);
    sum_of_field_test "sum of float values in computation_database.json"
      "computation_database.json" "Types" "float" 39.3;
    mean_of_field_test
      "mean of float values in computation_database.json"
      "computation_database.json" "Types" "float" 13.1;
    computation_of_any_field_test
      "concatenation of string values in computation_database.json"
      "computation_database.json" "Types" "string" identity "" ( ^ )
      "cs3110\\n";
    computation_of_any_field_test
      "and concatenation of bool values in computation_database.json"
      "computation_database.json" "Types" "bool" bool_of_string true
      ( && ) false;
    computation_of_any_field_test
      "or concatenation of bool values in computation_database.json"
      "computation_database.json" "Types" "bool" bool_of_string false
      ( || ) true;
    computation_of_any_field_test
      "multiplication of of int values in computation_database.json"
      "computation_database.json" "Types" "int" int_of_string 1 ( * )
      (-1965600);
    computation_of_any_field_test
      "concatenation with extra effects of boundary string values in \
       computation_database.json"
      "computation_database.json" "Mismatch" "Weird Notation" identity
      "START"
      (fun acc s -> acc ^ "; " ^ s ^ "; ")
      "START; \\\"; ; ''; ; yes; ; ; ";
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
          sort_field_string "test_database3.json" "users" "age" compare)
    );
    ( "sort_field_int for test_database3.json where field is not file"
    >:: fun _ ->
      assert_raises (ValNotFound "4") (fun () ->
          sort_field_int "test_database3.json" "test" "4" compare) );
    ( "sort_field_int for test_database3.json where string cannot be \
       converted to int"
    >:: fun _ ->
      assert_raises CannotConvertToNum (fun () ->
          sort_field_int "test_database3.json" "Users" "age" compare) );
    ( "sort_field_general for test_database3.json where converting \
       function does not do a good job of converting"
    >:: fun _ ->
      assert_raises CannotConvertElement (fun () ->
          sort_field_general "test_database3.json" "test" "2"
            bool_of_string string_of_bool compare) );
    ( "sum_of_field for computation_database.json where values cannot \
       be converted to floats"
    >:: fun _ ->
      assert_raises CannotConvertToNum (fun () ->
          sum_of_field "computation_database.json" "Mismatch" "1") );
    ( "mean_of_field for computation_database.json where values cannot \
       be converted to floats"
    >:: fun _ ->
      assert_raises CannotConvertToNum (fun () ->
          mean_of_field "computation_database.json" "Mismatch" "1") );
    ( "computation_of_any_field for computation_database.json where \
       values cannot be converted to integers"
    >:: fun _ ->
      assert_raises CannotConvertElement (fun () ->
          computation_of_any_field "computation_database.json"
            "Mismatch" "Int mismatch" int_of_string 0 ( + )) );
    ( "computation_of_any_field for computation_database.json where \
       values cannot be converted to integers even though they may \
       look like they should"
    >:: fun _ ->
      assert_raises CannotConvertElement (fun () ->
          computation_of_any_field "computation_database.json"
            "Mismatch" "Max/Min" int_of_string 0 ( + )) );
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           first_section_tests;
           second_section_tests;
           third_section_tests;
           computation_tests;
           exception_tests;
         ]

let _ = run_test_tt_main suite
