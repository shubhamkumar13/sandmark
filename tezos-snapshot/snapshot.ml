let download_file dest =
        let download_url = "https://docs.google.com/uc?export=download&id=145TXbrO1mNKU3gQefxd_Hpj0zesfllY9" in
        let url_with_cookies = Printf.sprintf "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate '%s' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\\1\\n/p')&id=145TXbrO1mNKU3gQefxd_Hpj0zesfllY9" download_url in
        let wget_url = Printf.sprintf "wget --load-cookies /tmp/cookies.txt \"%s\" -O snapshot.full && rm -rf /tmp/cookies.txt" url_with_cookies in
        Sys.command wget_url |> fun _ -> ()

let clone_tezos dest_tezos =
        let tezos_repo = "https://gitlab.com/tezos/tezos.git" in
        let clone_repo =
                let _ = Sys.command @@ Printf.sprintf "cd %s && git clone %s" dest_tezos tezos_repo in
                let _ = Sys.command @@ Printf.sprintf "mkdir -p %stezos/data" dest_tezos in
                () in
        try Sys.is_directory @@ Printf.sprintf "%s/tezos" dest_tezos |> fun _ ->
            Sys.command @@ Printf.sprintf "rm -rf %stezos" dest_tezos |> fun _ ->
            clone_repo
        with Sys_error _ -> clone_repo

let _ =
        let dest_snapshot = "./snapshot.full" in
        let dest_tezos = "../benchmarks/snapshot_import/" in
        let is_snapshot = Sys.file_exists dest_snapshot in
        clone_tezos dest_tezos;
        match is_snapshot with
        | true  -> Printf.printf "Snapshot is present in the directory\n"
        | false -> download_file dest_snapshot
