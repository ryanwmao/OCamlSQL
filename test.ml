open OUnit2
open Readcsv
open Table

(** TEST PLAN: Our test suite tests almost all of the functions that are
    used in our table.ml file, which are essentially the SQL functions
    that are called on the top level. We used a mixture of black box and
    white box testing to ensure that our functions work properly. The
    majority of test cases are simple tests to ensure our system's base
    functionality. We created and found online csv and txt files written
    in csv format to test the functions written in table.ml. We mostly
    used airtravel.csv and testing.txt for our testing purposes. White
    box testing was first performed on the individual helper functions
    that our SQL functions used to check over certain edge cases and
    solve anticipated/existing issues before testing the overall main
    functions. We used black box testing to check the main functions in
    order to test the overall functionality of our system and that the
    interaction between everything works. We mainly tested the table.ml
    functions in our test suite, because we manually tested our
    parsing/lexing functionality by running our program and seeing if
    our system performed the correct operations given acceptable
    commands. However, we could only test these capabilities after
    knowing that the functions that our system uses work properly.
    Hence, we tested the table.ml functions first in our OUnit test
    suite before moving onto manual testing. Our OUnit test suite also
    ensures the functionality of our readcsv.ml file, as most of our
    tests use a mixture of functions form table.ml and readcsv.ml at the
    same time. These functions were relatively simple to test as we only
    had to check if we could properly read data from a given file and
    print our own data to a specified file. Thus, we tested our file
    parsing and printing capabilities by reading data from various
    files, using that data to run the helper and main functions in
    table.ml and then print the resulting output onto various files and
    checking if everything is correct. Our Ast.ml, command_parser.ml,
    database.ml, eval.ml, and parser_main.ml files were mainly tested
    manually in the top level. As we already checked over our system's
    base functionality by testing readcsv and table in our OUnit test
    suite, we knew that errors from testing in the top level were most
    likely due to errors in the parsing/lexing/interpreting files or due
    to issues in how we integrated everything. While our OUnit test
    suite is not exhaustive, it adequately demonstrates our
    implementations of SQL functions and file operations. Thus, we
    believe our OUnit test suite and manual testing on the top level are
    thorough enough to guarantee the correctness of our system, as we
    thoroughly tested the individual components of our system separately
    using the OUnit test suite and tested the overall interactions
    between our components through the manual testing. *)
let readcsv_test name t e : test =
  name >:: fun _ -> assert_equal e (export_string t)

let read_csv_test name t a1 a2 e : test =
  name >:: fun _ -> assert_equal e (String.sub (export_string t) a1 a2)

let readcsv_empty_test name t e : test =
  name >:: fun _ -> assert_raises e t

let airtravel_data = from_csv "airtravel.csv"

let numbers_data = from_csv "numbers.txt"

let numbers_string = "\n1, 8\n2, 9\n3, 0\n4, 1\n5, 2\n6, 3\n7, 4"

let numbers_test_string = "\n1, 2, 3, 4, 5, 6, 7\n8, 9, 0, 1, 2, 3, 4"

let testing_data = from_csv "testing.txt"

let exports =
  [
    export_csv airtravel_data "airtravel_test";
    export_csv numbers_data "numbers_test";
    export_csv empty "empty_test";
    export_csv testing_data "testing_test";
  ]

let reading_tests =
  [
    read_csv_test "read airtravel.csv col 1" airtravel_data 1 65
      "Month, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, \
       DEC";
    read_csv_test "read airtravel.csv col 2" airtravel_data 67 63
      "1958, 340, 318, 362, 348, 363, 435, 491, 505, 404, 359, 310, 33";
    read_csv_test "read airtravel.csv col 3" airtravel_data 132 64
      "1959, 360, 342, 406, 396, 420, 472, 548, 559, 463, 407, 362, 405";
    read_csv_test "read airtravel.csv col 4" airtravel_data 197 64
      "1960, 417, 391, 419, 461, 472, 535, 622, 606, 508, 461, 390, 432";
    readcsv_test "read numbers.txt" numbers_data numbers_string;
    read_csv_test "read testing.txt col 1" testing_data 1 53
      "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16";
    read_csv_test "read testing.txt" testing_data 55 115
      "my, nice, cool, Kennedy, jack, house, kitchen, league, pennies, \
       cowboy, Michael, Ocaml, Hat, Neeko, Samira, Daisuki";
    read_csv_test "read testing.txt col 3" testing_data 171 94
      "6000, 4000, 9000, 2000, 1000, 0000, 8000, 5000, 3000, 2800, \
       3090, 0010, 2900, 8090, 7409, 2222";
    read_csv_test "read testing.txt col 4" testing_data 266 111
      "wowzers, ok, Jacob, Donlon, rain, hail, omnivamp, legend, \
       stones, cs, Clarkson, Sad, Bat, coward, Brolit, seven";
    readcsv_test "read empty" empty "";
    readcsv_test "read numbers export"
      (from_csv "numbers_test")
      numbers_test_string;
    readcsv_empty_test "read empty export"
      (fun () -> from_csv "empty_test")
      End_of_file;
  ]

let col_of_int_test name c i e : test =
  name >:: fun _ -> assert_equal e (col_of_int c i)

let airtravel_int_col = convert_to_c (Array.make 13 (string_of_int 2))

let airtravel_cols =
  select
    [ from_csv "airtravel.csv" ]
    [ "Month"; "1958"; "1959"; "1960" ]

let testing_int_col = convert_to_c (Array.make 16 (string_of_int 2))

let testing_cols =
  select [ from_csv "testing.txt" ] [ "1"; "my"; "6000"; "wowzers" ]

let select_test name tbls cols e : test =
  name >:: fun _ -> assert_equal e (select tbls cols)

let airtravel_1958 =
  convert_to_c
    (Array.of_list
       [
         "1958"; "340"; "318"; "362"; "348"; "363"; "435"; "491"; "505"; "404";
         "359"; "310"; "337";
       ]) [@ocamlformat "disable"]

let column_relation_test name c e : test =
  name >:: fun _ -> assert_equal e c

let airtravel_false_column =
  Array.of_list
    [
      false; false; false; false; false; false; false; false; false; false;
      false; false; false;
    ] [@ocamlformat "disable"]

let airtravel_true_column =
  Array.of_list
    [
      true; true; true; true; true; true; true; true; true; true; true; true;
      true;
    ] [@ocamlformat "disable"]

let month_less_1958 =
  List.nth airtravel_cols 3 <: List.nth airtravel_cols 2

let airtravel_1958_less_1959 =
  List.nth airtravel_cols 2 <: List.nth airtravel_cols 1

let month_greater_1958 =
  List.nth airtravel_cols 3 >: List.nth airtravel_cols 2

let airtravel_1958_greater_1959 =
  List.nth airtravel_cols 2 >: List.nth airtravel_cols 1

let month_less_equal_1958 =
  List.nth airtravel_cols 3 <: List.nth airtravel_cols 2

let airtravel_1958_less_equal_1959 =
  List.nth airtravel_cols 2 <=: List.nth airtravel_cols 1

let month_greater_equal_1958 =
  List.nth airtravel_cols 3 >=: List.nth airtravel_cols 2

let airtravel_1958_greater_equal_1959 =
  List.nth airtravel_cols 2 >=: List.nth airtravel_cols 1

let month_equal = List.nth airtravel_cols 3 =: List.nth airtravel_cols 3

let airtravel_1958_equal =
  List.nth airtravel_cols 2 =: List.nth airtravel_cols 2

let airtravel_1958_equal_Month =
  List.nth airtravel_cols 2 =: List.nth airtravel_cols 3

let airtravel_1958_equal_1959 =
  List.nth airtravel_cols 2 =: List.nth airtravel_cols 1

let month_not_equal_1958 =
  !=:(List.nth airtravel_cols 3) (List.nth airtravel_cols 2)

let month_not_equal_month =
  !=:(List.nth airtravel_cols 3) (List.nth airtravel_cols 3)

let airtravel_1958_not_equal_1958 =
  !=:(List.nth airtravel_cols 2) (List.nth airtravel_cols 2)

let wowzers_less_my = List.nth testing_cols 0 <: List.nth testing_cols 2

let wowzers_less_my_bool =
  Array.of_list
    [
      false; false; true; true; false; true; false; false; false; false; true;
      false; true; false; true; false;
    ] [@ocamlformat "disable"]

let wowzers_greater_my =
  List.nth testing_cols 0 >: List.nth testing_cols 2

let wowzers_greater_my_bool =
  Array.of_list
    [
      true; true; false; false; true; false; true; true; true; true; false;
      true; false; true; false; true;
    ] [@ocamlformat "disable"]

let wowzers_less_equal_my =
  List.nth testing_cols 0 <=: List.nth testing_cols 2

let wowzers_less_equal_my_bool =
  Array.of_list
    [
      false; false; true; true; false; true; false; false; false; false; true;
      false; true; false; true; false;
    ] [@ocamlformat "disable"]

let wowzers_greater_equal_my =
  List.nth testing_cols 0 >=: List.nth testing_cols 2

let wowzers_greater_equal_my_bool =
  Array.of_list
    [
      true; true; false; false; true; false; true; true; true; true; false;
      true; false; true; false; true;
    ] [@ocamlformat "disable"]

let wowzers_equal_my =
  List.nth testing_cols 0 =: List.nth testing_cols 2

let wowzers_equal_wowzers =
  List.nth testing_cols 0 =: List.nth testing_cols 0

let wowzers_equal_my_bool =
  Array.of_list
    [
      false; false; false; false; false; false; false; false; false; false;
      false; false; false; false; false; false;
    ] [@ocamlformat "disable"]

let wowzers_not_equal_my =
  !=:(List.nth testing_cols 0) (List.nth testing_cols 2)

let wowzers_not_equal_wowzers =
  !=:(List.nth testing_cols 0) (List.nth testing_cols 0)

let wowzers_not_equal_my_bool =
  Array.of_list
    [
      true; true; true; true; true; true; true; true; true; true; true; true;
      true; true; true; true;
    ] [@ocamlformat "disable"]

let select_and_column_tests =
  [
    col_of_int_test "airtravel int columns 2"
      (List.hd airtravel_cols)
      2 airtravel_int_col;
    col_of_int_test "testing.txt int columns 2" (List.hd testing_cols) 2
      testing_int_col;
    select_test "select 1958 from airtravel"
      [ from_csv "airtravel.csv" ]
      [ "1958" ] [ airtravel_1958 ];
    column_relation_test "Month <: 1958 = false" month_less_1958
      airtravel_false_column;
    column_relation_test "1958 <: 1959 = true" airtravel_1958_less_1959
      airtravel_true_column;
    column_relation_test "Month >: 1958 = true" month_greater_1958
      airtravel_true_column;
    column_relation_test "1958 >: 1959 = false"
      airtravel_1958_greater_1959 airtravel_false_column;
    column_relation_test "Month <=: 1958 = false" month_less_equal_1958
      airtravel_false_column;
    column_relation_test "1958 <=: 1959 = true"
      airtravel_1958_less_equal_1959 airtravel_true_column;
    column_relation_test "Month >=: 1958 = true"
      month_greater_equal_1958 airtravel_true_column;
    column_relation_test "1958 >=: 1959 = false"
      airtravel_1958_greater_equal_1959 airtravel_false_column;
    column_relation_test "Month =: Month = true" month_equal
      airtravel_true_column;
    column_relation_test "1958 =: 1958 = true" airtravel_1958_equal
      airtravel_true_column;
    column_relation_test "1958 =: Month = false"
      airtravel_1958_equal_Month airtravel_false_column;
    column_relation_test "1958 =: 1959 = false"
      airtravel_1958_equal_1959 airtravel_false_column;
    column_relation_test "!=: month 1958 = true" month_not_equal_1958
      airtravel_true_column;
    column_relation_test "!=: month month = false" month_not_equal_month
      airtravel_false_column;
    column_relation_test "!=: 1958 1958 = false"
      airtravel_1958_not_equal_1958 airtravel_false_column;
    column_relation_test "wowzers <: my" wowzers_less_my
      wowzers_less_my_bool;
    column_relation_test "wowzers >: my" wowzers_greater_my
      wowzers_greater_my_bool;
    column_relation_test "wowzers <=: my" wowzers_less_equal_my
      wowzers_less_equal_my_bool;
    column_relation_test "wowzers >=: my" wowzers_greater_equal_my
      wowzers_greater_equal_my_bool;
    column_relation_test "wowzers =: my" wowzers_equal_my
      wowzers_equal_my_bool;
    column_relation_test "wowzers =: wowzers" wowzers_equal_wowzers
      wowzers_not_equal_my_bool;
    column_relation_test "!=: wowzers my" wowzers_not_equal_my
      wowzers_not_equal_my_bool;
    column_relation_test "!=: wowzers wowzers" wowzers_not_equal_wowzers
      wowzers_equal_my_bool;
  ]

let where_col_filter_test name arr c e : test =
  name >:: fun _ -> assert_equal e (where_col_filter arr c)

let column_bop_test name c e : test = name >:: fun _ -> assert_equal e c

let testing_1_plus_6000 =
  List.nth testing_cols 1 +: List.nth testing_cols 3

let testing_1_plusf_6000 =
  List.nth testing_cols 1 +.: List.nth testing_cols 3

let testing_1_plus_6000_res =
  convert_to_c 
    (Array.of_list 
      [ 
        "4002"; "9003"; "2004"; "1005"; "6"; "8007"; "5008"; "3009"; 
        "2810"; "3101"; "22"; "2913"; "8104"; "7424"; "2238" 
      ]) [@ocamlformat "disable"]

let testing_1_plusf_6000_res =
  convert_to_c 
    (Array.of_list 
    [ 
      "4002."; "9003."; "2004."; "1005."; "6."; "8007."; "5008."; 
      "3009."; "2810."; "3101."; "22."; "2913."; "8104."; "7424."; "2238."
     ]) [@ocamlformat "disable"]

let testing_6000_minus_1 =
  List.nth testing_cols 1 -: List.nth testing_cols 3

let testing_6000_minusf_1 =
  List.nth testing_cols 1 -.: List.nth testing_cols 3

let testing_6000_minus_1_res =
  convert_to_c
    (Array.of_list 
      [
        "3998"; "8997"; "1996"; "995"; "-6"; "7993"; "4992"; "2991"; 
        "2790"; "3079"; "-2"; "2887"; "8076"; "7394"; "2206"
      ]) [@ocamlformat "disable"]

let testing_6000_minusf_1_res =
  convert_to_c
    (Array.of_list 
      [
        "3998."; "8997."; "1996."; "995."; "-6."; "7993."; "4992."; "2991."; 
        "2790."; "3079."; "-2."; "2887."; "8076."; "7394."; "2206."
      ]) [@ocamlformat "disable"]

let testing_1_times_6000 =
  List.nth testing_cols 1 *: List.nth testing_cols 3

let testing_1_timesf_6000 =
  List.nth testing_cols 1 *.: List.nth testing_cols 3

let testing_1_times_6000_res =
  convert_to_c
    (Array.of_list
      [
        "8000"; "27000"; "8000"; "5000"; "0"; "56000"; "40000"; "27000";
        "28000"; "33990"; "120"; "37700"; "113260"; "111135"; "35552"
      ]) [@ocamlformat "disable"]

let testing_1_timesf_6000_res =
  convert_to_c
  (Array.of_list
    [
      "8000."; "27000."; "8000."; "5000."; "0."; "56000."; "40000."; 
      "27000."; "28000."; "33990."; "120."; "37700."; "113260."; "111135."; 
      "35552."
    ]) [@ocamlformat "disable"]

let testing_6000_divide_1 =
  List.nth testing_cols 1 /: List.nth testing_cols 3

let testing_6000_dividef_1 =
  List.nth testing_cols 1 /.: List.nth testing_cols 3

let testing_6000_divide_1_res =
  convert_to_c
    (Array.of_list
      [
        "2000"; "3000"; "500"; "200"; "0"; "1142"; "625"; "333"; "280";
        "280"; "0"; "223"; "577"; "493"; "138"
      ]) [@ocamlformat "disable"]

let testing_6000_dividef_1_res =
  convert_to_c
  (Array.of_list
    [
      "2000."; "3000."; "500."; "200."; "0."; "1142.85714286"; "625.";
      "333.333333333"; "280."; "280.909090909"; "0.833333333333";
      "223.076923077"; "577.857142857"; "493.933333333"; "138.875"
    ]) [@ocamlformat "disable"]

let testing_6000_mod_1 =
  List.nth testing_cols 1 %: List.nth testing_cols 3

let testing_6000_mod_1_res =
  convert_to_c
    (Array.of_list
      [
        "0"; "0"; "0"; "0"; "0"; "6"; "0"; "3"; "0"; "10"; "10"; "1"; "12";
        "14"; "14"
      ]) [@ocamlformat "disable"]

let function_of_int_test name c fx e : test =
  name >:: fun _ -> assert_equal e (function_of_int c fx)

let function_of_float_test name c fx e : test =
  name >:: fun _ -> assert_equal e (function_of_float c fx)

let square x = x * x

let squaref x = x *. x

let testing_6000_square =
  convert_to_c
    (Array.of_list
      [
        "16000000"; "81000000"; "4000000"; "1000000"; "0"; 
        "64000000";"25000000"; "9000000"; "7840000"; "9548100"; "100"; 
        "8410000"; "65448100"; "54893281"; "4937284"
      ]) [@ocamlformat "disable"]

let testing_6000_squaref =
  convert_to_c
    (Array.of_list
      [
        "16000000."; "81000000."; "4000000."; "1000000."; "0.";
        "64000000."; "25000000."; "9000000."; "7840000."; "9548100."; "100.";
        "8410000."; "65448100."; "54893281."; "4937284."
      ]) [@ocamlformat "disable"]

let where_and_column_operations_tests =
  [
    where_col_filter_test "wcf false month" airtravel_false_column
      (List.nth airtravel_cols 3)
      (convert_to_c [||]);
    where_col_filter_test "wcf true month" airtravel_true_column
      (List.nth airtravel_cols 3)
      (List.nth airtravel_cols 3);
    where_col_filter_test "wcf wowzers_less_my wowzers"
      wowzers_less_my_bool
      (List.nth testing_cols 0)
      (convert_to_c
         (Array.of_list
            [ "Jacob"; "Donlon"; "hail"; "Clarkson"; "Bat"; "Brolit" ]));
    where_col_filter_test "wcf wowzers_less_my my" wowzers_less_my_bool
      (List.nth testing_cols 2)
      (convert_to_c
         (Array.of_list
            [ "cool"; "Kennedy"; "house"; "Michael"; "Hat"; "Samira" ]));
    column_bop_test "1 +: 6000" testing_1_plus_6000
      testing_1_plus_6000_res;
    column_bop_test "1 +.: 6000" testing_1_plusf_6000
      testing_1_plusf_6000_res;
    column_bop_test "6000 -: 1" testing_6000_minus_1
      testing_6000_minus_1_res;
    column_bop_test "6000 -.: 1" testing_6000_minus_1
      testing_6000_minus_1_res;
    column_bop_test "1 *: 6000" testing_1_times_6000
      testing_1_times_6000_res;
    column_bop_test "6000 *.: 1" testing_1_timesf_6000
      testing_1_timesf_6000_res;
    column_bop_test "6000 /: 1" testing_6000_divide_1
      testing_6000_divide_1_res;
    column_bop_test "6000 /.: 1" testing_6000_dividef_1
      testing_6000_dividef_1_res;
    column_bop_test "6000  %: 1" testing_6000_mod_1
      testing_6000_mod_1_res;
    function_of_int_test "square 6000"
      (List.nth testing_cols 1)
      square testing_6000_square;
    function_of_float_test "squaref 6000"
      (List.nth testing_cols 1)
      squaref testing_6000_squaref;
  ]

(*let order_by_test name asc tbl c e : test = name >:: fun _ ->
  assert_equal e (export_string (order_by asc tbl c))*)

let orderby_test name asc tbl c a1 a2 e : test =
  name >:: fun _ ->
  assert_equal e (String.sub (export_string (order_by asc tbl c)) a1 a2)

let order_by_tests =
  [
    orderby_test "order by 1958 asc col 1" true
      (from_csv "airtravel.csv")
      "1958" 1 65
      "Month, NOV, FEB, DEC, JAN, APR, OCT, MAR, MAY, SEP, JUN, JUL, \
       AUG";
    orderby_test "order by 1958 asc col 2" true
      (from_csv "airtravel.csv")
      "1958" 67 64
      "1958, 310, 318, 337, 340, 348, 359, 362, 363, 404, 435, 491, 505";
    orderby_test "order by 1958 asc col 3" true
      (from_csv "airtravel.csv")
      "1958" 132 64
      "1959, 362, 342, 405, 360, 396, 407, 406, 420, 463, 472, 548, 559";
    orderby_test "order by 1958 asc col 4" true
      (from_csv "airtravel.csv")
      "1958" 197 64
      "1960, 390, 391, 432, 417, 461, 461, 419, 472, 508, 535, 622, 606";
    orderby_test "order by 1958 desc col 1" false
      (from_csv "airtravel.csv")
      "1958" 1 65
      "Month, AUG, JUL, JUN, SEP, MAY, MAR, OCT, APR, JAN, DEC, FEB, \
       NOV";
    orderby_test "order by 1958 desc col 2" false
      (from_csv "airtravel.csv")
      "1958" 67 64
      "1958, 505, 491, 435, 404, 363, 362, 359, 348, 340, 337, 318, 310";
    orderby_test "order by 1958 desc col 3" false
      (from_csv "airtravel.csv")
      "1958" 132 64
      "1959, 559, 548, 472, 463, 420, 406, 407, 396, 360, 405, 342, 362";
    orderby_test "order by 1958 desc col 4" false
      (from_csv "airtravel.csv")
      "1958" 197 64
      "1960, 606, 622, 535, 508, 472, 419, 461, 461, 417, 432, 391, 390";
    orderby_test "order by month asc col 1" true
      (from_csv "airtravel.csv")
      "Month" 1 65
      "Month, APR, AUG, DEC, FEB, JAN, JUL, JUN, MAR, MAY, NOV, OCT, \
       SEP";
    orderby_test "order by month asc col 2" true
      (from_csv "airtravel.csv")
      "Month" 67 64
      "1958, 348, 505, 337, 318, 340, 491, 435, 362, 363, 310, 359, 404";
    orderby_test "order by month asc col 3" true
      (from_csv "airtravel.csv")
      "Month" 132 64
      "1959, 396, 559, 405, 342, 360, 548, 472, 406, 420, 362, 407, 463";
    orderby_test "order by month asc col 4" true
      (from_csv "airtravel.csv")
      "Month" 197 64
      "1960, 461, 606, 432, 391, 417, 622, 535, 419, 472, 390, 461, 508";
    orderby_test "order by month desc col 1" false
      (from_csv "airtravel.csv")
      "Month" 1 65
      "Month, SEP, OCT, NOV, MAY, MAR, JUN, JUL, JAN, FEB, DEC, AUG, \
       APR";
    orderby_test "order by month desc col 2" false
      (from_csv "airtravel.csv")
      "Month" 67 64
      "1958, 404, 359, 310, 363, 362, 435, 491, 340, 318, 337, 505, 348";
    orderby_test "order by month desc col 3" false
      (from_csv "airtravel.csv")
      "Month" 132 64
      "1959, 463, 407, 362, 420, 406, 472, 548, 360, 342, 405, 559, 396";
    orderby_test "order by month desc col 4" false
      (from_csv "airtravel.csv")
      "Month" 197 64
      "1960, 508, 461, 390, 472, 419, 535, 622, 417, 391, 432, 606, 461";
    orderby_test "order by 6000 asc col 1" true
      (from_csv "testing.txt")
      "6000" 1 53
      "1, 6, 12, 5, 4, 16, 10, 13, 9, 11, 2, 8, 15, 7, 14, 3";
    orderby_test "order by 6000 asc col 2" true
      (from_csv "testing.txt")
      "6000" 55 115
      "my, house, Ocaml, jack, Kennedy, Daisuki, cowboy, Hat, pennies, \
       Michael, nice, league, Samira, kitchen, Neeko, cool";
    orderby_test "order by 6000 asc col 3" true
      (from_csv "testing.txt")
      "6000" 171 94
      "6000, 0000, 0010, 1000, 2000, 2222, 2800, 2900, 3000, 3090, \
       4000, 5000, 7409, 8000, 8090, 9000";
    orderby_test "order by 6000 asc col 4" true
      (from_csv "testing.txt")
      "6000" 266 111
      "wowzers, hail, Sad, rain, Donlon, seven, cs, Bat, stones, \
       Clarkson, ok, legend, Brolit, omnivamp, coward, Jacob";
    orderby_test "order by 6000 desc col 1" false
      (from_csv "testing.txt")
      "6000" 1 53
      "1, 3, 14, 7, 15, 8, 2, 11, 9, 13, 10, 16, 4, 5, 12, 6";
    orderby_test "order by 6000 desc col 2" false
      (from_csv "testing.txt")
      "6000" 55 115
      "my, cool, Neeko, kitchen, Samira, league, nice, Michael, \
       pennies, Hat, cowboy, Daisuki, Kennedy, jack, Ocaml, house";
    orderby_test "order by 6000 desc col 3" false
      (from_csv "testing.txt")
      "6000" 171 94
      "6000, 9000, 8090, 8000, 7409, 5000, 4000, 3090, 3000, 2900, \
       2800, 2222, 2000, 1000, 0010, 0000";
    orderby_test "order by 6000 desc col 4" false
      (from_csv "testing.txt")
      "6000" 266 111
      "wowzers, Jacob, coward, omnivamp, Brolit, legend, ok, Clarkson, \
       stones, Bat, cs, seven, Donlon, rain, Sad, hail";
  ]

let group_no_aggregate_test name t c t2 c2 e : test =
  name >:: fun _ ->
  assert_equal (convert_to_c e)
    (group_no_aggregate t c (bins_of_col t2 c2))

let group_no_agg_1958 =
  [|
    "1958"; "318"; "505"; "491"; "340"; "348"; "359"; "435"; "404"; "363"; "337";
    "362"; "310"
  |] [@ocamlformat "disable"]

let group_no_agg_Month =
  [|
    "Month"; "APR"; "JAN"; "NOV"; "OCT"; "FEB"; "JUN"; "AUG"; "MAY"; "SEP";
    "JUL"; "DEC"; "MAR"
  |] [@ocamlformat "disable"]

let group_no_agg_1958_bins_month =
  [|
    "1958"; "348"; "340"; "310"; "359"; "318"; "435"; "505"; "363"; "404"; "491";
    "337"; "362"
  |] [@ocamlformat "disable"]

let group_no_agg_6000_bins_Month =
  [|
    "6000"; "1000"; "4000"; "0010"; "3090"; "9000"; "8000"; "3000"; "0000";
    "2800"; "5000"; "2900"; "2000"
  |] [@ocamlformat "disable"]

let group_by_tests =
  [
    group_no_aggregate_test "group no agg 1958"
      (from_csv "airtravel.csv")
      "1958"
      (from_csv "airtravel.csv")
      "1958" group_no_agg_1958;
    group_no_aggregate_test "group no agg Month"
      (from_csv "airtravel.csv")
      "Month"
      (from_csv "airtravel.csv")
      "Month" group_no_agg_Month;
    group_no_aggregate_test "group no agg 1958 bins Month"
      (from_csv "airtravel.csv")
      "1958"
      (from_csv "airtravel.csv")
      "Month" group_no_agg_1958_bins_month;
    group_no_aggregate_test "group no agg 6000 bins Month"
      (from_csv "testing.txt")
      "6000"
      (from_csv "airtravel.csv")
      "Month" group_no_agg_6000_bins_Month;
  ]

let count_test name s a e : test =
  name >:: fun _ -> assert_equal e (count s a)

let sum_int_test name s1 s2 e : test =
  name >:: fun _ -> assert_equal e (sum_int s1 s2)

let sum_float_test name s1 s2 e : test =
  name >:: fun _ -> assert_equal e (sum_float s1 s2)

let min_test name a1 a2 e : test =
  name >:: fun _ -> assert_equal e (min a1 a2)

let max_test name a1 a2 e : test =
  name >:: fun _ -> assert_equal e (max a1 a2)

let extra_functions_tests =
  [
    count_test "count 4 = 5" "4" 4 "5";
    sum_int_test "sum 5, 6 = 11" "5" "6" "11";
    sum_float_test "sum 500000000000, 600000000000 = 1100000000000"
      "500000000000" "600000000000" "1.1e+12";
    min_test "min 5 4 = 4 (int)" 5 4 4;
    min_test "min 5 4 = 4 (string)" "5" "4" "4";
    max_test "max 5 4 = 5 (int)" 5 4 5;
    max_test "max 5 4 = 5 (string)" "5" "4" "5";
  ]

let suite =
  "search test suite"
  >::: List.flatten
         [
           reading_tests;
           select_and_column_tests;
           where_and_column_operations_tests;
           order_by_tests;
           group_by_tests;
           extra_functions_tests;
         ]

let _ =
  run_test_tt_main suite;
  Sys.remove "airtravel_test";
  Sys.remove "numbers_test";
  Sys.remove "empty_test";
  Sys.remove "testing_test"

(* let size_test name d e : test = name >:: fun _ -> assert_equal e
   (size d)

   let tests = [ size_test "size 1 dictionary" size1Dictionary 1; ]

   let suite = "search test suite" >::: List.flatten [
   listDictionary_tests ]

   let _ = run_test_tt_main suite *)
