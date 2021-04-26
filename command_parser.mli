type query_tokens

val tokenizer: string -> query_tokens

val print_query_tokens: query_tokens -> unit

val select_tokens: query_tokens -> string list

val from_tokens: query_tokens -> string list

val where_tokens: query_tokens -> string list

val group_by_tokens: query_tokens -> string list

val order_by_tokens: query_tokens -> string list