mars_rover
=====

An OTP application. Mars rovers that move on a 10x10 canvas, controlled by authorised owners, not allowed to run into each other.

OTP supervision tree:
```
└── mars_rover_app
    └── mars_rover_sup
        ├── grid
        └── rover
```
APIs:

```erlang
{ok, Pid} = rover:new(Id, {X,Y}),
ok = rover:assume_control(Pid),
{ok, {X, Y}} = rover:north(Pid),
{ok, {X, Y}} = rover:south(Pid),
{ok, {X, Y}} = rover:east(Pid),
{ok, {X, Y}} = rover:west(Pid).
```


Build
-----

    $ rebar3 compile

Running
-----

    $ rebar3 shell


```erlang
Erlang/OTP 22 [erts-10.6.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.6.1  (abort with ^G)
1> rover:new(rover1, {0,0}).
{ok,<0.137.0>}
2> rover:new(rover2, {0,1}).
{ok,<0.139.0>}
3> rover:new(rover3, {0,2}).
{ok,<0.141.0>}
4> rover:new(rover4, {0,3}).
{ok,<0.143.0>}
5> rover:new(rover5, {0,3}).
{error,occupied}
6> rover:new(rover5, {0,4}).
{ok,<0.148.0>}
7> sys:get_state(grid).
{grid_state,[{{0,4},<0.148.0>},
             {{0,3},<0.143.0>},
             {{0,2},<0.141.0>},
             {{0,1},<0.139.0>},
             {{0,0},<0.137.0>}]}
8> rover:north(list_to_pid("<0.137.0>")).
{error,forbidden}
9> rover:assume_control(list_to_pid("<0.137.0>")).
ok
10> rover:north(list_to_pid("<0.137.0>")).
Rover moved successfully to 1, 0
{ok,{1,0}}
11> rover:east(list_to_pid("<0.137.0>")).
Rover moved successfully to 1, 1
{ok,{1,1}}
12> rover:south(list_to_pid("<0.137.0>")).
Rover unable to move. Still at 1, 1
{ok,{1,1}}
13> rover:assume_control(list_to_pid("<0.139.0>")).
ok
14> rover:west(list_to_pid("<0.139.0>")).
Rover moved successfully to 0, 0
{ok,{0,0}}
15> rover:south(list_to_pid("<0.137.0>")).
Rover moved successfully to 0, 1
{ok,{0,1}}
16> sys:get_state(grid).
{grid_state,[{{0,1},<0.137.0>},
             {{0,0},<0.139.0>},
             {{0,4},<0.148.0>},
             {{0,3},<0.143.0>},
             {{0,2},<0.141.0>}]}
17>

```