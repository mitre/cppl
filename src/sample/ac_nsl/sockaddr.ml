open Unix

let parse_hostport sa =
  let i =
    try
      String.index sa ':'
    with Not_found ->
      failwith ("no colon in address: " ^ sa) in
  let host = String.sub sa 0 i in
  let p = String.sub sa (i + 1) (String.length sa - i - 1) in
  let port =
    try
      int_of_string p
    with Failure s ->
      failwith ("bad port: " ^ p) in
  host, port

let mk_sockaddr (host, port) =
  let host_entry =
    try
      gethostbyname host
    with Not_found ->
      failwith ("host not found: " ^ host) in
  let inet_addr =
    try
      host_entry.h_addr_list.(0)
    with Invalid_argument s ->
      failwith("no addresses associated with host: " ^ host) in
  ADDR_INET (inet_addr, port)

let sockaddr str =
  mk_sockaddr (parse_hostport str)
