os_stats
=====

`os_stats` aim to collect linux system information such as kernel vsn, loadavg, disk, memory usage, cpu utilization, IO statistics.

It also provides a plugin for observer_cli.

Example
=====

```erlang
1>os_stats:kernel_vsn().
"2.6.32-504.el6.x86_64"
2> os_stats:loadavg().
#{avg1 => 0.23,avg15 => 0.1,avg5 => 0.15,p_last_pid => 29373,
  p_running => 1,p_threads => 785,
  raw_bin => <<"0.23 0.15 0.10 1/785 29373\n">>}
3> {os_stats:avg1(),os_stats:avg5(), os_stats:avg15()}.
{0.33,0.2,0.12}
4> os_stats:uptime().
#{human_uptime =>
      #{days => 3,hours => 4,mins => 18,secs => 40},
  idletime => 4237044.47,uptime => 274719.86}
5> os_stats:disk().
{ok,[#{available => 418599600128,capacity => 3,
       files_system => "/dev/mapper/VolGroup-lv_home",
       mount => "/home",size => 450870231040,type => "ext4",
       used => 9360924672},
     #{available => 209223680,capacity => 1,
       files_system => "/dev/sda1",mount => "/boot/efi",
       size => 209489920,type => "vfat",used => 266240},
     #{available => 438453248,capacity => 8,
       files_system => "/dev/sda2",mount => "/boot",
       size => 499355648,type => "ext4",used => 34688000},
     #{available => 33712881664,capacity => 0,
       files_system => "tmpfs",mount => "/dev/shm",
       size => 33712881664,type => "tmpfs",used => 0},
     #{available => 51859554304,capacity => 49,
       files_system => "/dev/mapper/VolGroup-lv_root",mount => "/",
       size => 105557286912,type => "ext4",used => 48328814592}]}
6> os_stats:memory().
{ok,#{buffers => 265760,cached => 34460248,mem_free => 28102360,
      mem_total => 65845472,swap_cached => 0,
      swap_free => 32989180,swap_total => 32989180}}

7> {ok, PrevDiskStat} = os_stats:disk_stat().
{ok,[#{dev_name => 'dm-2',ios_pgr => 0,rd_ios => 393,
       rd_merges => 0,rd_secs => 3138,rd_ticks => 2301,
       rq_ticks => 2304,
       timestamp => {1550,646958,567568},
       tot_ticks => 212,wr_ios => 3,wr_merges => 0,wr_secs => 24,
       wr_ticks => 3},
     #{dev_name => 'dm-1',ios_pgr => 0,rd_ios => 324,
       rd_merges => 0,rd_secs => 2592,rd_ticks => 518,
       rq_ticks => 518,
       timestamp => {1550,646958,567492},
       tot_ticks => 358,wr_ios => 0,wr_merges => 0,wr_secs => 0,
       wr_ticks => 0},
     #{dev_name => 'dm-0',ios_pgr => 0,rd_ios => 58410,
       rd_merges => 0,rd_secs => 943554,rd_ticks => 338704,
       rq_ticks => 1440798529,
       timestamp => {1550,646958,567416},
       tot_ticks => 8423557,wr_ios => 24599307,wr_merges => 0,
       wr_secs => 196794456,wr_ticks => 1440328539},
     #{dev_name => sda,ios_pgr => 0,rd_ios => 42599,
       rd_merges => 18352,rd_secs => 978878,rd_ticks => 272820,
       rq_ticks => 41749964,
       timestamp => {1550,646958,567169},
       tot_ticks => 8423848,wr_ios => 3017785,
       wr_merges => 21581489,wr_secs => 196794498,
       wr_ticks => 41477748}]}
8> {ok, NewDiskStat} = os_stats:disk_stat().
{ok,[#{dev_name => 'dm-2',ios_pgr => 0,rd_ios => 393,
       rd_merges => 0,rd_secs => 3138,rd_ticks => 2301,
       rq_ticks => 2304,
       timestamp => {1550,647027,602667},
       tot_ticks => 212,wr_ios => 3,wr_merges => 0,wr_secs => 24,
       wr_ticks => 3},
     #{dev_name => 'dm-1',ios_pgr => 0,rd_ios => 324,
       rd_merges => 0,rd_secs => 2592,rd_ticks => 518,
       rq_ticks => 518,
       timestamp => {1550,647027,602598},
       tot_ticks => 358,wr_ios => 0,wr_merges => 0,wr_secs => 0,
       wr_ticks => 0},
     #{dev_name => 'dm-0',ios_pgr => 0,rd_ios => 58410,
       rd_merges => 0,rd_secs => 943554,rd_ticks => 338704,
       rq_ticks => 1440831027,
       timestamp => {1550,647027,602527},
       tot_ticks => 8426337,wr_ios => 24602753,wr_merges => 0,
       wr_secs => 196822024,wr_ticks => 1440361037},
     #{dev_name => sda,ios_pgr => 0,rd_ios => 42599,
       rd_merges => 18352,rd_secs => 978878,rd_ticks => 272820,
       rq_ticks => 41757515,
       timestamp => {1550,647027,602281},
       tot_ticks => 8426629,wr_ios => 3018753,
       wr_merges => 21583967,wr_secs => 196822066,
       wr_ticks => 41485299}]}
9> os_stats:disk_stat_diff(NewDiskStat, PrevDiskStat).
[#{dev_name => sda,rd_secs => 0,rd_secs_ps => 0.0,
   timestamp => {1550,647027,602281},
   tps => 19.86,wr_secs => 21416,wr_secs_ps => 862.75},
 #{dev_name => 'dm-0',rd_secs => 0,rd_secs_ps => 0.0,
   timestamp => {1550,647027,602527},
   tps => 107.84,wr_secs => 21416,wr_secs_ps => 862.75},
 #{dev_name => 'dm-1',rd_secs => 0,rd_secs_ps => 0.0,
   timestamp => {1550,647027,602598},
   tps => 0.0,wr_secs => 0,wr_secs_ps => 0.0},
 #{dev_name => 'dm-2',rd_secs => 0,rd_secs_ps => 0.0,
   timestamp => {1550,647027,602667},
   tps => 0.0,wr_secs => 0,wr_secs_ps => 0.0}]

10> {ok, PrevIfStat} = os_stats:if_stat().
{ok,[#{dev_name => lo,rx_bytes => 19739163269,rx_dropped => 0,
       rx_errors => 0,rx_packets => 16072781,
       timestamp => {1550,647207,825629},
       tx_bytes => 19739163269,tx_dropped => 0,tx_errors => 0,
       tx_packets => 16072781},
     #{dev_name => eth0,rx_bytes => 88366078169,rx_dropped => 0,
       rx_errors => 0,rx_packets => 176353439,
       timestamp => {1550,647207,825917},
       tx_bytes => 92110748617,tx_dropped => 0,tx_errors => 0,
       tx_packets => 230339388},
     #{dev_name => eth1,rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647207,826176},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0},
     #{dev_name => eth2,rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647207,826455},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0},
     #{dev_name => eth3,rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647207,826729},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0},
     #{dev_name => virbr0,rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647207,826995},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0},
     #{dev_name => 'virbr0-nic',rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647207,827252},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0}]}
11> {ok, NewIfStat} = os_stats:if_stat().
{ok,[#{dev_name => lo,rx_bytes => 19740096665,rx_dropped => 0,
       rx_errors => 0,rx_packets => 16073510,
       timestamp => {1550,647217,266452},
       tx_bytes => 19740096665,tx_dropped => 0,tx_errors => 0,
       tx_packets => 16073510},
     #{dev_name => eth0,rx_bytes => 88366143949,rx_dropped => 0,
       rx_errors => 0,rx_packets => 176354008,
       timestamp => {1550,647217,266736},
       tx_bytes => 92110817993,tx_dropped => 0,tx_errors => 0,
       tx_packets => 230339726},
     #{dev_name => eth1,rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647217,267002},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0},
     #{dev_name => eth2,rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647217,267252},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0},
     #{dev_name => eth3,rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647217,267524},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0},
     #{dev_name => virbr0,rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647217,267770},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0},
     #{dev_name => 'virbr0-nic',rx_bytes => 0,rx_dropped => 0,
       rx_errors => 0,rx_packets => 0,
       timestamp => {1550,647217,268002},
       tx_bytes => 0,tx_dropped => 0,tx_errors => 0,
       tx_packets => 0}]}
12> os_stats:if_stat_diff(NewIfStat,PrevIfStat).
[#{dev_name => 'virbr0-nic',rx_bps => 0.0,rx_pps => 0.0,
   timestamp => {1550,647217,268002},
   tx_bps => 0.0,tx_pps => 0.0},
 #{dev_name => virbr0,rx_bps => 0.0,rx_pps => 0.0,
   timestamp => {1550,647217,267770},
   tx_bps => 0.0,tx_pps => 0.0},
 #{dev_name => eth3,rx_bps => 0.0,rx_pps => 0.0,
   timestamp => {1550,647217,267524},
   tx_bps => 0.0,tx_pps => 0.0},
 #{dev_name => eth2,rx_bps => 0.0,rx_pps => 0.0,
   timestamp => {1550,647217,267252},
   tx_bps => 0.0,tx_pps => 0.0},
 #{dev_name => eth1,rx_bps => 0.0,rx_pps => 0.0,
   timestamp => {1550,647217,267002},
   tx_bps => 0.0,tx_pps => 0.0},
 #{dev_name => eth0,rx_bps => 6967.62,rx_pps => 60.27,
   timestamp => {1550,647217,266736},
   tx_bps => 7348.51,tx_pps => 35.8},
 #{dev_name => lo,rx_bps => 98868.08,rx_pps => 77.22,
   timestamp => {1550,647217,266452},
   tx_bps => 98868.08,tx_pps => 77.22}]

13> {ok, PrevCpuStat} = os_stats:cpu_stat().
{ok,[#{core => 0,hardware_interrupts => 8,idle => 25851737,
       software_interrupts => 18858,steal_time => 0,
       system => 281884,user => 990224,user_nice => 4,
       wait => 269252},
       #{core => avg,hardware_interrupts => 8,idle => 25851737,
       software_interrupts => 18858,steal_time => 0,
       system => 281884,user => 990224,user_nice => 4,
       wait => 269252}]}.
14> {ok, NewCpuStat} = os_stats:cpu_stat().
{ok,[#{core => 0,hardware_interrupts => 8,idle => 25852421,
       software_interrupts => 18858,steal_time => 0,
       system => 281888,user => 990228,user_nice => 4,
       wait => 269260},
       #{core => avg,hardware_interrupts => 8,idle => 25852421,
         software_interrupts => 18858,steal_time => 0,
         system => 281888,user => 990228,user_nice => 4,
         wait => 269260}]}
15> CpuStatDiff = os_stats:cpu_stat_diff(NewCpuStat,PrevCpuStat).
[#{core => avg,hardware_interrupts => 0,idle => 684,
   software_interrupts => 0,steal_time => 0,system => 4,
   user => 4,user_nice => 0,wait => 8},
 #{core => 0,hardware_interrupts => 0,idle => 684,
   software_interrupts => 0,steal_time => 0,system => 4,
   user => 4,user_nice => 0,wait => 8}]
16> os_stats:cputime_util(CpuStatDiff).
[#{core => avg,hardware_interrupts_util => 0.0,
   idle_util => 0.9771,software_interrupts_util => 0.0,
   steal_time_util => 0.0,system_util => 0.0057,
   user_nice_util => 0.0,user_util => 0.0057,
   wait_util => 0.0114},
 #{core => 0,hardware_interrupts_util => 0.0,
   idle_util => 0.9771,software_interrupts_util => 0.0,
   steal_time_util => 0.0,system_util => 0.0057,
   user_nice_util => 0.0,user_util => 0.0057,
   wait_util => 0.0114}]

```

Plugin observer_cli
=====

`sys.config`

```erlang
[{observer_cli, {plugins,[#{module => os_stats_plug, title => "OS", interval => 2000, shortcut => "O", sort_column => 2}]}]
```

![PluginPic](https://user-images.githubusercontent.com/3116225/53075587-86eaa780-3528-11e9-9140-38d060d329c9.jpg)
