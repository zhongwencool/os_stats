-module(os_stats).

%% API exports
-export([kernel_vsn/0, uptime/0]).
-export([loadavg/0, avg1/0, avg5/0, avg15/0, nprocs/0]).
-export([memory/0, disk/0]).
-export([proc_stat/0, disk_stat/0, cpu_stat/0, if_stat/0, if_stat/1]).
-export([disk_stat_diff/2, if_stat_diff/2, cpu_stat_diff/2]).
-export([cputime_util/1]).

-define(ProcLoadAvg, "/proc/loadavg").
-define(ProcStat, "/proc/stat").
-define(ProcUptime, "/proc/uptime").
-define(ProcVersion, "/proc/version").
-define(ProcMemInfo, "/proc/meminfo").
-define(ProcDiskStats, "/proc/diskstats").
-define(NetDevice, "/sys/class/net/").
-define(TS_FACTOR, 1000000).

%%====================================================================
%% API functions
%%====================================================================

kernel_vsn() ->
    read_stats_from_file(?ProcVersion,
        fun(Bin) ->
            case re:run(Bin, "^Linux version (.*?) ",
                [{capture, all_but_first, list}]) of
                {match, [Vsn | _]} -> Vsn;
                nomatch -> {error, {badarg, Bin}}
            end
        end).

loadavg() ->
    read_stats_from_file(?ProcLoadAvg,
        fun(Bin) ->
            %%   <<"0.03 0.03 0.00 3/336 7286\n">>
            [Avg1, Avg5, Avg15, PRun, PTot, PLast, <<>>] =
                binary:split(Bin, [<<" ">>, <<"/">>, <<"\n">>], [global]),
            #{
                avg1 => binary_to_float(Avg1),
                avg5 => binary_to_float(Avg5),
                avg15 => binary_to_float(Avg15),
                p_running => binary_to_integer(PRun),
                p_threads => binary_to_integer(PTot),
                p_last_pid => binary_to_integer(PLast),
                raw_bin => Bin
            }
        end).

avg1() ->
    read_stats_from_file(?ProcLoadAvg,
        fun(Bin) ->
            [Avg1, _] = binary:split(Bin, [<<" ">>], []),
            binary_to_float(Avg1)
        end).

avg5() ->
    read_stats_from_file(?ProcLoadAvg,
        fun(Bin) ->
            [_Avg1, Avg5 | _] = binary:split(Bin, [<<" ">>], [global]),
            binary_to_float(Avg5)
        end).

avg15() ->
    read_stats_from_file(?ProcLoadAvg,
        fun(Bin) ->
            [_Avg1, _Avg5, Avg15 | _] = binary:split(Bin, [<<" ">>], [global]),
            binary_to_float(Avg15)
        end).

nprocs() ->
    read_stats_from_file(?ProcLoadAvg,
        fun(Bin) ->
            %% <<"0.03 0.03 0.00 3/336 7286\n">>
            [_Avg1, _Avg5, _Avg15, _PRun, PTot, _PLast] =
                binary:split(Bin, [<<" ">>, <<"/">>], [global]),
            binary_to_integer(PTot)
        end).

uptime() ->
    read_stats_from_file(?ProcUptime,
        fun(Bin) ->
            %%   <<"8290696.22 132068396.43\n">>
            [UptimeBin, IdleTimeBin, <<>>] = binary:split(Bin, [<<" ">>, <<"\n">>], [global]),
            Uptime = binary_to_float(UptimeBin),
            #{
                uptime => Uptime,
                idletime => binary_to_float(IdleTimeBin),
                human_uptime => uptime_to_human(Uptime)
            }
        end).

memory() ->
    read_stats_from_file(?ProcMemInfo,
        fun(Bin, State) ->
            case re:split(Bin, "\\s+") of
                [<<"MemTotal:">>, Val, _, _] ->
                    {ok, maps:put(mem_total, binary_to_integer(Val), State)};
                [<<"MemFree:">>, Val, _, _] ->
                    {ok, maps:put(mem_free, binary_to_integer(Val), State)};
                [<<"Buffers:">>, Val, _, _] ->
                    {ok, maps:put(buffers, binary_to_integer(Val), State)};
                [<<"Cached:">>, Val, _, _] ->
                    {ok, maps:put(cached, binary_to_integer(Val), State)};
                [<<"SwapCached:">>, Val, _, _] ->
                    {ok, maps:put(swap_cached, binary_to_integer(Val), State)};
                [<<"SwapTotal:">>, Val, _, _] ->
                    {ok, maps:put(swap_total, binary_to_integer(Val), State)};
                [<<"SwapFree:">>, Val, _, _] ->
                    {eof, maps:put(swap_free, binary_to_integer(Val), State)};
                _ ->
                    {ok, State}
            end
        end
        , #{}).

disk() ->
    List = os:cmd("df -T -P -B1"),
    {ok, disk_storage_from_df(List)}.

proc_stat() ->
    read_stats_from_file(?ProcStat, fun parse_proc_stat_line/2, #{cpu => []}).

cpu_stat() ->
    read_stats_from_file(?ProcStat, fun parse_proc_stat_line/2, []).

disk_stat() ->
    read_stats_from_file(?ProcDiskStats, fun parse_disk_stat_line/2, []).

if_stat() ->
    {ok, [if_stat(Dev) || Dev <- net_devices()]}.

if_stat(Device) ->
    #{
        dev_name => Device,
        rx_bytes => read_net_dev_stats(Device, "/statistics/rx_bytes"),
        tx_bytes => read_net_dev_stats(Device, "/statistics/tx_bytes"),
        rx_packets => read_net_dev_stats(Device, "/statistics/rx_packets"),
        tx_packets => read_net_dev_stats(Device, "/statistics/tx_packets"),
        rx_errors => read_net_dev_stats(Device, "/statistics/rx_errors"),
        tx_errors => read_net_dev_stats(Device, "/statistics/tx_errors"),
        rx_dropped => read_net_dev_stats(Device, "/statistics/rx_dropped"),
        tx_dropped => read_net_dev_stats(Device, "/statistics/tx_dropped"),
        timestamp => os:timestamp()
    }.

disk_stat_diff(DiskStatsY, DiskStatsX) ->
    disk_stat_diff(DiskStatsY, DiskStatsX, []).

if_stat_diff(IfStatsY, IfStatsX) ->
    if_stat_diff(IfStatsY, IfStatsX, []).

cpu_stat_diff(CpuY, CpuX) ->
    cpu_stat_diff(CpuY, CpuX, []).

cputime_util(Cpus) when is_list(Cpus) ->
    [begin cputime_util(C) end || C <- Cpus];
cputime_util(Cpu) ->
    #{
        core := Core,
        user := Us, system := Sy, user_nice := Ni, idle := Id, wait := Wa,
        hardware_interrupts := Hi, software_interrupts := Si, steal_time := St
    } = Cpu,
    Total = Us + Sy + Ni + Id + Wa + Hi + Si + St,
    #{
        core => Core,
        user_util => round((Us / Total) * 10000) / 10000,
        system_util => round((Sy / Total) * 10000) / 10000,
        user_nice_util => round((Ni / Total) * 10000) / 10000,
        idle_util => round((Id / Total) * 10000) / 10000,
        wait_util => round((Wa / Total) * 10000) / 10000,
        hardware_interrupts_util => round((Hi / Total) * 10000) / 10000,
        software_interrupts_util => round((Si / Total) * 10000) / 10000,
        steal_time_util => round((St / Total) * 10000) / 10000
    }.

%%====================================================================
%% Internal functions
%%====================================================================

disk_stat_diff([], _, Acc) -> Acc;
disk_stat_diff([DiskStatY | T], DiskStatsX, Matched) ->
    #{dev_name := Dev} = DiskStatY,
    case search_dev(Dev, DiskStatsX, []) of
        {DiskStatX, NewDiskStatsX} ->
            IoStat = disk_stat_to_io_stat(DiskStatY, DiskStatX),
            disk_stat_diff(T, NewDiskStatsX, [IoStat | Matched]);
        false ->
            disk_stat_diff(T, DiskStatsX, Matched)
    end.

if_stat_diff([], _, Acc) -> Acc;
if_stat_diff([IfStatY | T], IfStatsX, Matched) ->
    #{dev_name := Dev} = IfStatY,
    case search_dev(Dev, IfStatsX, []) of
        {IfStatX, NewIfStatsX} ->
            IoStat = if_stat_to_net_stat(IfStatY, IfStatX),
            if_stat_diff(T, NewIfStatsX, [IoStat | Matched]);
        false ->
            if_stat_diff(T, IfStatsX, Matched)
    end.

cpu_stat_diff([], _, Acc) -> Acc;
cpu_stat_diff([CpuStatY | T], CpuStatsX, Matched) ->
    #{core := Core} = CpuStatY,
    case search_core(Core, CpuStatsX, []) of
        {CpuStatX, NewCpuStatsX} ->
            Stat = cpu_stat_to_cputime_stat(CpuStatY, CpuStatX),
            cpu_stat_diff(T, NewCpuStatsX, [Stat | Matched]);
        false ->
            cpu_stat_diff(T, CpuStatsX, Matched)
    end.

if_stat_to_net_stat(IfStatsY, IfStatsX) ->
    #{
        dev_name := Dev,
        rx_bytes := RxBY, tx_bytes := TxBY,
        rx_packets := RxPY, tx_packets := TxPY,
        timestamp := TimeStampY
    } = IfStatsY,
    #{
        rx_bytes := RxBX, tx_bytes := TxBX,
        rx_packets := RxPX, tx_packets := TxPX,
        timestamp := TimeStampX
    } = IfStatsX,
    TDelta = timer:now_diff(TimeStampY, TimeStampX),
    #{
        dev_name => Dev,
        rx_bps => round(((RxBY - RxBX) * ?TS_FACTOR / TDelta) * 100) / 100,
        tx_bps => round(((TxBY - TxBX) * ?TS_FACTOR / TDelta) * 100) / 100,
        rx_pps => round(((RxPY - RxPX) * ?TS_FACTOR / TDelta) * 100) / 100,
        tx_pps => round(((TxPY - TxPX) * ?TS_FACTOR / TDelta) * 100) / 100,
        timestamp => TimeStampY
    }.

disk_stat_to_io_stat(DiskStatY, DiskStatX) ->
    #{
        dev_name := DevName,
        timestamp := TimestampY, rd_ios := RdIosY, wr_ios := WrIosY,
        rd_secs := RdSecsY, wr_secs := WrSecsY
    } = DiskStatY,
    #{
        timestamp := TimestampX, rd_ios := RdIosX, wr_ios := WrIosX,
        rd_secs := RdSecsX, wr_secs := WrSecsX
    } = DiskStatX,
    TDelta = timer:now_diff(TimestampY, TimestampX),
    TC = (RdIosY + WrIosY) - (RdIosX + WrIosX),
    TPS = round((TC * ?TS_FACTOR / TDelta) * 100) / 100,
    BlkRead = RdSecsY - RdSecsX,
    BlkReadRate = round((BlkRead * ?TS_FACTOR / TDelta) * 100) /100,
    BlkWrtn = WrSecsY - WrSecsX,
    BlkWrtnRate = round((BlkWrtn * ?TS_FACTOR / TDelta) * 100) / 100,
    #{
        dev_name => DevName,
        tps => TPS,
        rd_secs_ps => BlkReadRate,
        wr_secs_ps => BlkWrtnRate,
        rd_secs => BlkRead,
        wr_secs => BlkWrtn,
        timestamp => TimestampY
    }.

cpu_stat_to_cputime_stat(CpuStatY, CpuStatX) ->
    #{
        core := Core,
        user := UsY, system := SyY, user_nice := NiY, idle := IdY, wait := WaY,
        hardware_interrupts := HiY, software_interrupts := SiY, steal_time := StY
    } = CpuStatY,
    #{
        core := Core,
        user := UsX, system := SyX, user_nice := NiX, idle := IdX, wait := WaX,
        hardware_interrupts := HiX, software_interrupts := SiX, steal_time := StX
    } = CpuStatX,
    #{
        core => Core,
        user => UsY - UsX, system => SyY - SyX, user_nice => NiY - NiX,
        idle => IdY - IdX, wait => WaY - WaX,
        hardware_interrupts => HiY - HiX, software_interrupts => SiY - SiX,
        steal_time => StY - StX
    }.

search_dev(_Dev, [], _Res) -> false;
search_dev(Dev, [#{dev_name := Dev} = Target | List], Res) -> {Target, Res ++ List};
search_dev(Dev, [NotMatch | List], Res) -> search_dev(Dev, List, [NotMatch | Res]).

search_core(_Core, [], _Res) -> false;
search_core(Core, [#{core := Core} = Target | List], Res) -> {Target, Res ++ List};
search_core(Core, [NotMatch | List], Res) -> search_core(Core, List, [NotMatch | Res]).

disk_storage_from_df(List) ->
    Lines = tl(string:split(List, [<<"\n">>], all)),
    {ok, Regex} = re:compile("^(.*?)\\s+(.*?)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)%\\s+(.+)$"),
    disk_storage_parse_lines(Lines, Regex, []).

disk_storage_parse_lines([[]], _Regex, Acc) -> Acc;
disk_storage_parse_lines([Line | T], Regex, Acc) ->
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of
        nomatch ->
            disk_storage_parse_lines(T, Regex, Acc);
        {match, [FS, Type, Size, Used, Avail, Pct, Mnt]} ->
            DI = #{
                files_system => FS,
                type => Type,
                size => list_to_integer(Size),
                used => list_to_integer(Used),
                available => list_to_integer(Avail),
                capacity => list_to_integer(Pct),
                mount => Mnt
            },
            disk_storage_parse_lines(T, Regex, [DI | Acc])
    end.

uptime_to_human(UptimeFloat) ->
    Uptime = round(UptimeFloat),
    Days = Uptime div 86400,
    Hours = Uptime rem 86400 div 3600,
    Mins = Uptime rem 3600 div 60,
    Secs = Uptime rem 60,
    #{days => Days, hours => Hours, mins => Mins, secs => Secs}.

read_net_dev_stats(Device, Item) ->
    EntryPath = [?NetDevice, Device, Item],
    case file:read_file(EntryPath) of
        {ok, Bin} ->
            {Num, _Rest} = string:to_integer(binary_to_list(Bin)),
            Num;
        {error, _Reason} -> undefined
    end.

net_devices() ->
    case file:list_dir(?NetDevice) of
        {ok, Entries} -> [list_to_atom(E) || E <- Entries];
        {error, _Reason} -> []
    end.

parse_disk_stat_line(Bin, State) ->
    case re:split(Bin, "\\s+") of
        [
            <<>>, _Major, _Minor, Dev, RdIos, RdMerges, RdSecs, RdTicks,
            WrIos, WrMerges, WrSecs, WrTicks, IosPgr, TotTicks, RqTicks, <<>>
        ] ->
            case is_device(Dev) of
                true when RdIos =:= <<"0">>, WrIos =:= <<"0">> ->
                    {ok, State};
                true ->
                    {ok, [
                        #{
                            dev_name => binary_to_atom(Dev, latin1),
                            rd_ios => binary_to_integer(RdIos),
                            rd_merges => binary_to_integer(RdMerges),
                            rd_secs => binary_to_integer(RdSecs),
                            rd_ticks => binary_to_integer(RdTicks),
                            wr_ios => binary_to_integer(WrIos),
                            wr_merges=> binary_to_integer(WrMerges),
                            wr_secs => binary_to_integer(WrSecs),
                            wr_ticks => binary_to_integer(WrTicks),
                            ios_pgr => binary_to_integer(IosPgr),
                            tot_ticks => binary_to_integer(TotTicks),
                            rq_ticks => binary_to_integer(RqTicks),
                            timestamp => os:timestamp()
                        } | State]
                    };
                false ->
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end.

parse_proc_stat_line(Bin, State) ->
    {ok, CpuRe} = re:compile("cpu(\\d+)?\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)"),
    parse_proc_stat_line(Bin, CpuRe, State).

parse_proc_stat_line(<<"cpu", _/binary>> = Bin, Re, State) ->
    case re:run(Bin, Re, [{capture, all_but_first, list}]) of
        {match, [Tag | Nums]} ->
            [User, Nice, Sys, Idle, Wait, IRQ, SoftIRQ, Steal] = [list_to_integer(S) || S <- Nums],
            Core = if Tag =:= "" -> avg; true -> list_to_integer(Tag) end,
            Item = #{
                core => Core,
                user => User, system => Sys, user_nice => Nice, idle => Idle, wait => Wait,
                hardware_interrupts => IRQ, software_interrupts => SoftIRQ, steal_time => Steal
            },
            case State of
                #{cpu := PrevCpu} -> {ok, State#{cpu => [Item | PrevCpu]}};
                _ -> {ok, [Item | State]}
            end;
        _ -> {error, cpu_notmatch}
    end;
parse_proc_stat_line(<<"intr", _/binary>>, _, List) when is_list(List) ->
    {eof, List};
parse_proc_stat_line(<<"intr", _/binary>> = Line, _, State) ->
    {ok, Re} = re:compile("intr (\\d+) "),
    case re:run(Line, Re, [{capture, all_but_first, list}]) of
        {match, [Num]} ->
            {ok, maps:put(intr, list_to_integer(Num), State)};
        _ ->
            {error, inter_not_match}
    end;
parse_proc_stat_line(<<"softirq", _/binary>> = Line, _, State) ->
    {ok, Re} = re:compile("softirq (\\d+) "),
    case re:run(Line, Re, [{capture, all_but_first, list}]) of
        {match, [Num]} ->
            {ok, maps:put(softirq, list_to_integer(Num), State)};
        _ ->
            {error, softirq_notmatch}
    end;
parse_proc_stat_line(<<"ctxt", CTxt/binary>>, _, State) ->
    Num = binary_to_list(CTxt) -- " \n",
    {ok, maps:put(ctxt, list_to_integer(Num), State)};
parse_proc_stat_line(<<"btime", BTime/binary>>, _, State) ->
    Num = binary_to_list(BTime) -- " \n",
    {ok, maps:put(btime, list_to_integer(Num), State)};
parse_proc_stat_line(<<"processes", Processes/binary>>, _, State) ->
    Num = binary_to_list(Processes) -- " \n",
    {ok, maps:put(processes, list_to_integer(Num), State)};
parse_proc_stat_line(<<"procs_running", ProcessRun/binary>>, _, State) ->
    Num = binary_to_list(ProcessRun) -- " \n",
    {ok, maps:put(procs_running, list_to_integer(Num), State)};
parse_proc_stat_line(<<"procs_blocked", ProcsBlocked/binary>>, _, State) ->
    Num = binary_to_list(ProcsBlocked) -- " \n",
    {ok, maps:put(procs_blocked, list_to_integer(Num), State)};
parse_proc_stat_line(_, _, State) ->
    {ok, State}.

read_stats_from_file(File, Fun, State) ->
    case file:open(File, [read, binary]) of
        {ok, Fd} ->
            try
                parse_line(Fd, Fun, State)
            after
                file:close(Fd)
            end;
        Err -> Err
    end.

read_stats_from_file(File, Fun) ->
    case file:open(File, [read, binary]) of
        {ok, Fd} ->
            try
                case file:read_line(Fd) of
                    eof -> {ok, <<>>};
                    {error, _R} = E -> E;
                    {ok, Bin} -> Fun(Bin)
                end
            after
                file:close(Fd)
            end;
        Err -> Err
    end.

parse_line(Fd, Fun, Acc) ->
    case file:read_line(Fd) of
        eof -> {ok, Acc};
        {error, _R} = E -> E;
        {ok, Bin} ->
            case Fun(Bin, Acc) of
                {ok, NewAcc} ->
                    parse_line(Fd, Fun, NewAcc);
                {eof, NewAcc} ->
                    {ok, NewAcc}
            end
    end.

is_device(Item) ->
    Name =
        case re:run(Item, "^cciss/([0-9a-z]+)$", [{capture, all_but_first, list}]) of
            {match, [Partition]} -> "cciss!" ++ Partition;
            nomatch -> Item
        end,
    filelib:is_file(<<"/sys/block/", Name/binary>>).
