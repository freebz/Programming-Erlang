start() ->
    indexer_server:start(output_dir()),
    spawn_link(fun() -> worker() end).

worker() ->
    possibly_stop(),
    case indexer_server:next_dir() of
	{ok, Dir} ->
	    Files = indexer_misc:files_in_dir(Dir),
	    index_these_files(Files),
	    indexer_server:checkpoint(),
	    possibly_stop(),
	    sleep(10000),
	    worker();
	done ->
	    true
    end.


index_these_files(Files) ->
    Ets = indexer_server:ets_table(),
    OutDir = filename:join(indexer_server:outdir(), "index" ),
    F1 = fun(Pid, File) -> indexer_words:words_in_file(Pid, File, Ets) end,
    F2 = fun(Key, Val, Acc) -> handle_result(Key, Val, OutDir, Acc) end,
    indexer_misc:mapreduce(F1, F2, 0, Files).

handle_result(Key, Vals, OutDir, Acc) ->    
    add_to_file(OutDir, Key, Vals),
    Acc + 1.


add_to_fild(OutDir, Word, Is) ->
    L1 = map(fun(I) -> <<I:32>> end, Is),
    case file:open(OutFile, [write,binary,raw,append]) of
	{ok, S} ->
	    file:pwrite(S, 0, L1),
	    file:close(S);
	{error, E} ->
	    exit({ebadFileOp, OutFile, E})
    end.
