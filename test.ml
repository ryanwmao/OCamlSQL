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

let testing_1_plus_6000_res =
  convert_to_c 
    (Array.of_list 
      [ 
        "6001"; "4002"; "9003"; "2004"; "1005"; "6"; "8007"; "5008"; "3009"; 
        "2810"; "3101"; "22"; "2913"; "8104"; "7424"; "2238" 
      ]) [@ocamlformat "disable"]

let testing_6000_minus_1 =
  List.nth testing_cols 1 -: List.nth testing_cols 3

let testing_6000_minus_1_res =
  convert_to_c
    (Array.of_list 
      [
        "5999"; "3998"; "8997"; "1996"; "995"; "-6"; "7993"; "4992"; "2991"; 
        "2790"; "3079"; "-2"; "2887"; "8076"; "7394"; "2206"
      ]) [@ocamlformat "disable"]

let testing_1_times_6000 =
  List.nth testing_cols 1 *: List.nth testing_cols 3

let testing_1_times_6000_res =
  convert_to_c
    (Array.of_list
      [
        "6000"; "8000"; "27000"; "8000"; "5000"; "0"; "56000"; "40000"; "27000";
        "28000"; "33990"; "120"; "37700"; "113260"; "111135"; "35552"
      ]) [@ocamlformat "disable"]

let testing_6000_divide_1 =
  List.nth testing_cols 1 /: List.nth testing_cols 3

let testing_6000_divide_1_res =
  convert_to_c
    (Array.of_list
      [
        "6000"; "2000"; "3000"; "500"; "200"; "0"; "1142"; "625"; "333"; "280";
        "280"; "0"; "223"; "577"; "493"; "138"
      ]) [@ocamlformat "disable"]

let testing_6000_mod_1 =
  List.nth testing_cols 1 %: List.nth testing_cols 3

let testing_6000_mod_1_res =
  convert_to_c
    (Array.of_list
      [
        "0"; "0"; "0"; "0"; "0"; "0"; "6"; "0"; "3"; "0"; "10"; "10"; "1"; "12";
        "14"; "14"
      ]) [@ocamlformat "disable"]

let function_of_int_test name c fx e : test =
  name >:: fun _ -> assert_equal e (function_of_int c fx)

let square x = x * x

let testing_6000_square =
  convert_to_c
    (Array.of_list
      [
        "36000000"; "16000000"; "81000000"; "4000000"; "1000000"; "0"; 
        "64000000";"25000000"; "9000000"; "7840000"; "9548100"; "100"; 
        "8410000"; "65448100"; "54893281"; "4937284"
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
    column_bop_test "6000 -: 1" testing_6000_minus_1
      testing_6000_minus_1_res;
    column_bop_test "1 *: 6000" testing_1_times_6000
      testing_1_times_6000_res;
    column_bop_test "6000 /: 1" testing_6000_divide_1
      testing_6000_divide_1_res;
    column_bop_test "6000  %: 1" testing_6000_mod_1
      testing_6000_mod_1_res;
    function_of_int_test "square 6000"
      (List.nth testing_cols 1)
      square testing_6000_square;
  ]

let order_by_test name asc tbl c e : test =
  name >:: fun _ -> assert_equal e (export_string (order_by asc tbl c))

let order_by_1958_asc =
  "\n\
   Month, NOV, FEB, DEC, JAN, APR, OCT, MAR, MAY, SEP, JUN, JUL, AUG\n\
   1958, 310, 318, 337, 340, 348, 359, 362, 363, 404, 435, 491, 505\n\
   1959, 362, 342, 405, 360, 396, 407, 406, 420, 463, 472, 548, 559\n\
   1960, 390, 391, 432, 417, 461, 461, 419, 472, 508, 535, 622, 606"

let order_by_1958_desc =
  "\n\
   Month, AUG, JUL, JUN, SEP, MAY, MAR, OCT, APR, JAN, DEC, FEB, NOV\n\
   1958, 505, 491, 435, 404, 363, 362, 359, 348, 340, 337, 318, 310\n\
   1959, 559, 548, 472, 463, 420, 406, 407, 396, 360, 405, 342, 362\n\
   1960, 606, 622, 535, 508, 472, 419, 461, 461, 417, 432, 391, 390"

let order_by_Month_asc =
  "\n\
   Month, APR, AUG, DEC, FEB, JAN, JUL, JUN, MAR, MAY, NOV, OCT, SEP\n\
   1958, 348, 505, 337, 318, 340, 491, 435, 362, 363, 310, 359, 404\n\
   1959, 396, 559, 405, 342, 360, 548, 472, 406, 420, 362, 407, 463\n\
   1960, 461, 606, 432, 391, 417, 622, 535, 419, 472, 390, 461, 508"

let order_by_Month_desc =
  "\n\
   Month, SEP, OCT, NOV, MAY, MAR, JUN, JUL, JAN, FEB, DEC, AUG, APR\n\
   1958, 404, 359, 310, 363, 362, 435, 491, 340, 318, 337, 505, 348\n\
   1959, 463, 407, 362, 420, 406, 472, 548, 360, 342, 405, 559, 396\n\
   1960, 508, 461, 390, 472, 419, 535, 622, 417, 391, 432, 606, 461"

let order_by_6000_asc =
  "\n\
   1, 6, 12, 5, 4, 16, 10, 13, 9, 11, 2, 8, 15, 7, 14, 3\n\
   my, house, Ocaml, jack, Kennedy, Daisuki, cowboy, Hat, pennies, \
   Michael, nice, league, Samira, kitchen, Neeko, cool\n\
   6000, 0000, 0010, 1000, 2000, 2222, 2800, 2900, 3000, 3090, 4000, \
   5000, 7409, 8000, 8090, 9000\n\
   wowzers, hail, Sad, rain, Donlon, seven, cs, Bat, stones, Clarkson, \
   ok, legend, Brolit, omnivamp, coward, Jacob"

let order_by_6000_desc =
  "\n\
   1, 3, 14, 7, 15, 8, 2, 11, 9, 13, 10, 16, 4, 5, 12, 6\n\
   my, cool, Neeko, kitchen, Samira, league, nice, Michael, pennies, \
   Hat, cowboy, Daisuki, Kennedy, jack, Ocaml, house\n\
   6000, 9000, 8090, 8000, 7409, 5000, 4000, 3090, 3000, 2900, 2800, \
   2222, 2000, 1000, 0010, 0000\n\
   wowzers, Jacob, coward, omnivamp, Brolit, legend, ok, Clarkson, \
   stones, Bat, cs, seven, Donlon, rain, Sad, hail\n\
  \  "

let order_by_tests =
  [
    order_by_test "order by 1958 asc" true
      (from_csv "airtravel.csv")
      "1958" order_by_1958_asc;
    order_by_test "order by 1958 desc" false
      (from_csv "airtravel.csv")
      "1958" order_by_1958_desc;
    order_by_test "order by month asc" true
      (from_csv "airtravel.csv")
      "Month" order_by_Month_asc;
    order_by_test "order by month desc" false
      (from_csv "airtravel.csv")
      "Month" order_by_Month_desc;
    order_by_test "order by 6000 asc" true
      (from_csv "testing.txt")
      "6000" order_by_6000_asc;
    order_by_test "order by 6000 desc" false
      (from_csv "testing.txt")
      "6000" order_by_6000_desc;
  ]
(*let cleanup = [ Sys.remove "airtravel_test"; Sys.remove
  "numbers_test"; Sys.remove "empty_test"; ]*)

let suite =
  "search test suite"
  >::: List.flatten
         [
           reading_tests;
           select_and_column_tests;
           where_and_column_operations_tests;
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
