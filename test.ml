open OUnit2
open Readcsv
open Table

let readcsv_test name t e : test =
  name >:: fun _ -> assert_equal e (export_string t)

let readcsv_empty_test name t e : test =
  name >:: fun _ -> assert_raises e t

let airtravel_data = from_csv "airtravel.csv"

(*let airtravel_export = export_csv airtravel_data "airtravel_test"*)

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

let exports =
  [
    export_csv airtravel_data "airtravel_test";
    export_csv numbers_data "numbers_test";
    export_csv empty "empty_test";
  ]

let reading_tests =
  [
    readcsv_test "read airtravel.csv" airtravel_data airtravel_string;
    readcsv_test "read numbers.txt" numbers_data numbers_string;
    readcsv_test "read empty" empty "";
    readcsv_test "read airtravel export"
      (from_csv "airtravel_test")
      airtravel_test_string;
    readcsv_test "read numbers export"
      (from_csv "numbers_test")
      numbers_test_string;
    readcsv_empty_test "read empty export"
      (fun () -> from_csv "empty_test")
      End_of_file;
  ]

(*let cleanup = [ Sys.remove "airtravel_test"; Sys.remove
  "numbers_test"; Sys.remove "empty_test"; ]*)

let suite = "search test suite" >::: List.flatten [ reading_tests ]

let _ =
  run_test_tt_main suite;
  Sys.remove "airtravel_test";
  Sys.remove "numbers_test";
  Sys.remove "empty_test"

(* let size_test name d e : test = name >:: fun _ -> assert_equal e
   (size d)

   let tests = [ size_test "size 1 dictionary" size1Dictionary 1; ]

   let suite = "search test suite" >::: List.flatten [
   listDictionary_tests ]

   let _ = run_test_tt_main suite *)
