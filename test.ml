open OUnit2
open Readcsv
open Table

let readcsv_test name t e : test =
  name >:: fun _ -> assert_equal e (export_string t)

let readcsv_empty_test name t e : test =
  name >:: fun _ -> assert_raises e t

let airtravel_data = from_csv "airtravel.csv"

let airtravel_string =
  "\n\
   Month, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC\n\
   1958, 340, 318, 362, 348, 363, 435, 491, 505, 404, 359, 310, 337\n\
   1959, 360, 342, 406, 396, 420, 472, 548, 559, 463, 407, 362, 405\n\
   1960, 417, 391, 419, 461, 472, 535, 622, 606, 508, 461, 390, 432"

let airtravel_test_string =
  "\n\
   Month, 1958, 1959, 1960\n\
   JAN, 340, 360, 417\n\
   FEB, 318, 342, 391\n\
   MAR, 362, 406, 419\n\
   APR, 348, 396, 461\n\
   MAY, 363, 420, 472\n\
   JUN, 435, 472, 535\n\
   JUL, 491, 548, 622\n\
   AUG, 505, 559, 606\n\
   SEP, 404, 463, 508\n\
   OCT, 359, 407, 461\n\
   NOV, 310, 362, 390\n\
   DEC, 337, 405, 432"

let numbers_data = from_csv "numbers.txt"

(*let numbers_export = export_csv numbers_data "numbers_test"*)

let numbers_string = "\n1, 8\n2, 9\n3, 0\n4, 1\n5, 2\n6, 3\n7, 4"

let numbers_test_string = "\n1, 2, 3, 4, 5, 6, 7\n8, 9, 0, 1, 2, 3, 4"

let testing_data = from_csv "testing.txt"

let testing_string =
  "\n\
   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16\n\
   my, nice, cool, Kennedy, jack, house, kitchen, league, pennies, \
   cowboy, Michael, Ocaml, Hat, Neeko, Samira, Daisuki\n\
   6000, 4000, 9000, 2000, 1000, 0000, 8000, 5000, 3000, 2800, 3090, \
   0010, 2900, 8090, 7409, 2222\n\
   wowzers, ok, Jacob, Donlon, rain, hail, omnivamp, legend, stones, \
   cs, Clarkson, Sad, Bat, coward, Brolit, seven"

let testing_test_string =
  "\n\
   1, my, 6000, wowzers\n\
   2, nice, 4000, ok\n\
   3, cool, 9000, Jacob\n\
   4, Kennedy, 2000, Donlon\n\
   5, jack, 1000, rain\n\
   6, house, 0000, hail\n\
   7, kitchen, 8000, omnivamp\n\
   8, league, 5000, legend\n\
   9, pennies, 3000, stones\n\
   10, cowboy, 2800, cs\n\
   11, Michael, 3090, Clarkson\n\
   12, Ocaml, 0010, Sad\n\
   13, Hat, 2900, Bat\n\
   14, Neeko, 8090, coward\n\
   15, Samira, 7409, Brolit\n\
   16, Daisuki, 2222, seven"

let exports =
  [
    export_csv airtravel_data "airtravel_test";
    export_csv numbers_data "numbers_test";
    export_csv empty "empty_test";
    export_csv testing_data "testing_test";
  ]

let reading_tests =
  [
    readcsv_test "read airtravel.csv" airtravel_data airtravel_string;
    readcsv_test "read numbers.txt" numbers_data numbers_string;
    readcsv_test "read testing.txt" testing_data testing_string;
    readcsv_test "read empty" empty "";
    readcsv_test "read airtravel export"
      (from_csv "airtravel_test")
      airtravel_test_string;
    readcsv_test "read numbers export"
      (from_csv "numbers_test")
      numbers_test_string;
    readcsv_test "read testing export"
      (from_csv "testing_test")
      testing_test_string;
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
         "1958";
         "340";
         "318";
         "362";
         "348";
         "363";
         "435";
         "491";
         "505";
         "404";
         "359";
         "310";
         "337";
       ])

let column_relation_test name c e : test =
  name >:: fun _ -> assert_equal e c

let airtravel_false_column =
  Array.of_list
    [
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
    ]

let airtravel_true_column =
  Array.of_list
    [
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
    ]

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
      false;
      false;
      true;
      true;
      false;
      true;
      false;
      false;
      false;
      false;
      true;
      false;
      true;
      false;
      true;
      false;
    ]

let wowzers_greater_my =
  List.nth testing_cols 0 >: List.nth testing_cols 2

let wowzers_greater_my_bool =
  Array.of_list
    [
      true;
      true;
      false;
      false;
      true;
      false;
      true;
      true;
      true;
      true;
      false;
      true;
      false;
      true;
      false;
      true;
    ]

let wowzers_less_equal_my =
  List.nth testing_cols 0 <=: List.nth testing_cols 2

let wowzers_less_equal_my_bool =
  Array.of_list
    [
      false;
      false;
      true;
      true;
      false;
      true;
      false;
      false;
      false;
      false;
      true;
      false;
      true;
      false;
      true;
      false;
    ]

let wowzers_greater_equal_my =
  List.nth testing_cols 0 >=: List.nth testing_cols 2

let wowzers_greater_equal_my_bool =
  Array.of_list
    [
      true;
      true;
      false;
      false;
      true;
      false;
      true;
      true;
      true;
      true;
      false;
      true;
      false;
      true;
      false;
      true;
    ]

let wowzers_equal_my =
  List.nth testing_cols 0 =: List.nth testing_cols 2

let wowzers_equal_wowzers =
  List.nth testing_cols 0 =: List.nth testing_cols 0

let wowzers_equal_my_bool =
  Array.of_list
    [
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
      false;
    ]

let wowzers_not_equal_my =
  !=:(List.nth testing_cols 0) (List.nth testing_cols 2)

let wowzers_not_equal_wowzers =
  !=:(List.nth testing_cols 0) (List.nth testing_cols 0)

let wowzers_not_equal_my_bool =
  Array.of_list
    [
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
      true;
    ]

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
  ]
(*let cleanup = [ Sys.remove "airtravel_test"; Sys.remove
  "numbers_test"; Sys.remove "empty_test"; ]*)

let suite =
  "search test suite"
  >::: List.flatten
         [ reading_tests; select_and_column_tests; where_and_column_operations_tests ]

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
