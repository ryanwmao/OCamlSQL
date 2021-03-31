
type t = {
  mutable lines: string array array
}

let parseline str = failwith "Unimplemented"

let odd_quotes str = failwith "Unimplemented"

let readlines channel = 
  let read = ref [||] in
  try 
    while true do
      let line = ref (input_line channel) in 
      let newline = 
        if odd_quotes line
        then (while odd_quotes line do 
                line := !line ^ input_line channel
              done; !line)
        else !line in
      read := Array.append !read (Array.make 1 (parseline newline))
    done; !read
  with 
    End_of_file -> close_in_noerr channel; !read
  

let from_csv file = {
  lines = readlines file
}
