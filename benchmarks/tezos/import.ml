let _ =
  let _ = Sys.command @@ Printf.sprintf "tezos-node snapshot import /home/sk/sandmark/tezos-snapshot/snapshot.full --data-dir /home/sk/sandmark/benchmarks/tezos/tezos/data" in
  ()
