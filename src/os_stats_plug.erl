-module(os_stats_plug).

%% observer_cli_plugin Callback API
-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(PrevState) ->
    {PrevIfStat, PrevCpuStat} =
        case PrevState of
            undefined -> {undefined, undefined};
            #{ifstat := I, cpustat := C} -> {I, C}
        end,
    Sys =
        [
            #{content => "Linux version", width => 13}, #{content => os_stats:kernel_vsn(), width => 30},
            #{content => "Load average", width => 12}, #{content => loadavg(), width => 42},
            #{content => "Uptime", width => 11}, #{content => uptime(), width => 16, color => <<"\e[32;1m">>}
        ],
    Disk = disk_attrs(),
    Mem = memory_attrs(),
    {If, NewIfStats} = if_stats_attrs(PrevIfStat),
    {Cpu, NewCpuStats} = cpu_stats_attrs(PrevCpuStat),
    NewState = #{ifstat => NewIfStats, cpustat => NewCpuStats},
    {[Sys, Mem | Cpu] ++ If ++ Disk, NewState}.

sheet_header() ->
    [
        #{title => "DevName", width => 34},
        #{title => "TPS", width => 20},
        #{title => "BlkRead", width => 20},
        #{title => "BlkWrite", width => 20},
        #{title => "BlkReadRate", width => 20},
        #{title => "BlkWrtRate", width => 20}
    ].

sheet_body(undefined) ->
    {ok, PrevSheet} = os_stats:disk_stat(),
    timer:sleep(200),
    {ok, NewSheet} = os_stats:disk_stat(),
    {io_to_sheet(os_stats:disk_stat_diff(NewSheet, PrevSheet)), NewSheet};
sheet_body(PrevSheet) ->
    {ok, NewSheet} = os_stats:disk_stat(),
    {io_to_sheet(os_stats:disk_stat_diff(NewSheet, PrevSheet)), NewSheet}.

%%====================================================================
%% Internal functions
%%====================================================================

disk_attrs() ->
    {ok, DiskList} = os_stats:disk(),
    SortDisk = lists:sort(fun(#{capacity := C1}, #{capacity := C2}) -> C1 < C2 end, DiskList),
    DiskHeader =
        [
            #{content => "-", width => 3, color => <<"\e[7m">>},
            #{content => " Cap", width => 5, color => <<"\e[7m">>},
            #{content => " FileSystem", width => 37, color => <<"\e[7m">>},
            #{content => " Type", width => 14, color => <<"\e[7m">>},
            #{content => " Size", width => 14, color => <<"\e[7m">>},
            #{content => " Used", width => 14, color => <<"\e[7m">>},
            #{content => " Avail", width => 14, color => <<"\e[7m">>},
            #{content => " Mount", width => 31, color => <<"\e[7m">>}
        ],
    {_, Disk} =
        lists:foldl(fun(
            #{
                files_system := FS,
                type := Type,
                size := Size,
                used := Used,
                available := Avail,
                capacity := Pct,
                mount := Mnt
            }, {Len, Acc}
        ) ->
            {Len - 1,
                [[
                    
                    #{content => Len, width => 2}, #{content => integer_to_list(Pct) ++ "%", width => 3},
                    #{content => FS, width => 35}, #{content => Type, width => 12},
                    #{content => {byte, Size}, width => 12}, #{content => {byte, Used}, width => 12},
                    #{content => {byte, Avail}, width => 12}, #{content => Mnt, width => 30}
                ] | Acc]
            }
                    end, {length(SortDisk), []}, SortDisk),
    [DiskHeader | Disk].

memory_attrs() ->
    {ok, #{
        mem_total := MemTotal,
        mem_free := MemFree,
        buffers := Buffers,
        swap_cached := SwapCached,
        swap_total := SwapTotal
    }} = os_stats:memory(),
    [
        #{content => "MemTotal", width => 10}, #{content => {byte, MemTotal * 1024}, width => 12},
        #{content => "MemFree", width => 8}, #{content => {byte, MemFree * 1024}, width => 12},
        #{content => "Buffer", width => 10}, #{content => {byte, Buffers * 1024}, width => 12},
        #{content => "SwapTotal", width => 10}, #{content => {byte, SwapTotal * 1024}, width => 12},
        #{content => "SwapCached", width => 13}, #{content => {byte, SwapCached * 1024}, width => 13}
    ].

uptime() ->
    #{human_uptime := #{
        days := Days,
        hours := Hours,
        mins := Mins,
        secs := Secs}
    } = os_stats:uptime(),
    io_lib:format("~w days ~w:~w:~w", [Days, Hours, Mins, Secs]).

loadavg() ->
    #{raw_bin := RawBin} = os_stats:loadavg(),
    [Bin, _] = binary:split(RawBin, [<<"\n">>], [global]),
    Bin.

io_to_sheet(Maps) ->
    [begin
         #{
             dev_name := DevName,
             tps := TPS,
             rd_secs_ps := BlkReadRate,
             wr_secs_ps := BlkWrtnRate,
             rd_secs := BlkRead,
             wr_secs := BlkWrtn
         } = Map,
         [DevName, TPS, BlkRead, BlkWrtn, BlkReadRate, BlkWrtnRate]
     end || Map <- Maps].

if_stats_attrs(undefined) ->
    {ok, Prev} = os_stats:if_stat(),
    timer:sleep(200),
    {ok, New} = os_stats:if_stat(),
    {[if_header() | if_to_sheet(os_stats:if_stat_diff(New, Prev))], New};
if_stats_attrs(Prev) ->
    {ok, New} = os_stats:if_stat(),
    {[if_header() | if_to_sheet(os_stats:if_stat_diff(New, Prev))], New}.

if_header() ->
    [
        #{content => " DevName", width => 35, color => <<"\e[7m">>},
        #{content => " RxBytes/Sec", width => 25, color => <<"\e[7m">>},
        #{content => " TxBytes/Sec", width => 25, color => <<"\e[7m">>},
        #{content => " RxPackets/Sec", width => 25, color => <<"\e[7m">>},
        #{content => " TxPackets/Sec", width => 25, color => <<"\e[7m">>}
    ].

if_to_sheet(Maps) ->
    [begin
         #{
             dev_name := DevName,
             rx_bps := RxBps,
             tx_bps := TxBps,
             rx_pps := RxPps,
             tx_pps := TxPps
         } = Map,
         [
             #{content => DevName, width => 34},
             #{content => {byte, RxBps}, width => 23},
             #{content => {byte, TxBps}, width => 23},
             #{content => RxPps, width => 23},
             #{content => TxPps, width => 24}
         ]
     end || Map <- Maps].

cpu_stats_attrs(undefined) ->
    {ok, Prev} = os_stats:cpu_stat(),
    timer:sleep(200),
    {ok, New} = os_stats:cpu_stat(),
    Cpu = os_stats:cputime_util(os_stats:cpu_stat_diff(New, Prev)),
    {[cpu_header() | cpu_to_sheet(Cpu)], New};
cpu_stats_attrs(Prev) ->
    {ok, New} = os_stats:cpu_stat(),
    Cpu = os_stats:cputime_util(os_stats:cpu_stat_diff(New, Prev)),
    {[cpu_header() | cpu_to_sheet(Cpu)], New}.

cpu_header() ->
    [
        #{content => "Core", width => 4, color => <<"\e[7m">>},
        #{content => " User", width => 16, color => <<"\e[7m">>},
        #{content => " System", width => 16, color => <<"\e[7m">>},
        #{content => " UserNice", width => 16, color => <<"\e[7m">>},
        #{content => " Idle", width => 16, color => <<"\e[7m">>},
        #{content => " Wait", width => 16, color => <<"\e[7m">>},
        #{content => " HardwareIRQ", width => 16, color => <<"\e[7m">>},
        #{content => " SoftWareIRQ", width => 16, color => <<"\e[7m">>},
        #{content => " StealTime", width => 15, color => <<"\e[7m">>}
    ].

cpu_to_sheet(Maps) ->
    [begin
         #{
             core := Core,
             user_util := Us,
             system_util := Sys,
             user_nice_util := Ni,
             idle_util := Id,
             wait_util := Wa,
             hardware_interrupts_util := IRQ,
             software_interrupts_util := SIRQ,
             steal_time_util := St
         } = Map,
         [
             #{content => Core, width => 3},
             #{content => float_to_list(Us * 100, [{decimals, 2}]) ++ "%", width => 14},
             #{content => float_to_list(Sys * 100, [{decimals, 2}]) ++ "%", width => 14},
             #{content => float_to_list(Ni * 100, [{decimals, 2}]) ++ "%", width => 14},
             #{content => float_to_list(Id * 100, [{decimals, 2}]) ++ "%", width => 14},
             #{content => float_to_list(Wa * 100, [{decimals, 2}]) ++ "%", width => 14},
             #{content => float_to_list(IRQ * 100, [{decimals, 2}]) ++ "%", width => 14},
             #{content => float_to_list(SIRQ * 100, [{decimals, 2}]) ++ "%", width => 14},
             #{content => float_to_list(St * 100, [{decimals, 2}]) ++ "%", width => 14}
         ]
     end || Map <- Maps].
