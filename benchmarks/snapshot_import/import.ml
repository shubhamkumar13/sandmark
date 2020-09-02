let _ =
  let _ = Sys.command @@ Printf.sprintf "tezos-node snapshot import ../../tezos-snapshot/snapshot.full --data-dir ./tezos/data" in
  ()
