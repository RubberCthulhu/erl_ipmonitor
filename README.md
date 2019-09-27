ipmonitor - IP address monitor for Erlang
=========================================

Simple IP address monitor. It works a similar way to erlang:monitor/2 for processes and ports,
but it keep an eye on a given IP address and informs the caller with the message {ipmonitor, Monitor, Address, Status}
when the address appears or disappears in the system.

Usage
-----

    1> application:start(ipmonitor).
    ok
    2> ipmonitor:check("127.0.0.1").
    true
    3> ipmonitor:check("120.0.0.1").
    false 
    4> {ok, Monitor, Status} = ipmonitor:monitor("120.0.0.1", [{timeout, 5000}]).
    {ok,#Ref<0.2517935835.1081081857.242940>,false}
    5> ipmonitor:status(Monitor).
    false
    6> ipmonitor:info().
    #{#Ref<0.2517935835.1081081857.242940> =>
          #{addr => {120,0,0,1},
            name => #Ref<0.2517935835.1081081857.242940>,
            orig_addr => "120.0.0.1",pid => <0.61.0>,status => false}}
    7> ipmonitor:info(Monitor).
    #{addr => {120,0,0,1},
      name => #Ref<0.2517935835.1081081857.242940>,
      orig_addr => "120.0.0.1",pid => <0.61.0>,status => false}
    8> ipmonitor:demonitor(Monitor).
    ok

    1> application:start(ipmonitor).
    ok
    2> {ok, Monitor, Status} = ipmonitor:monitor("120.0.0.1").
    {ok,#Ref<0.457175728.2155610116.180474>,false}
    3> receive
    3>     {ipmonitor, Monitor, "120.0.0.1", true} ->
    3>         true;
    3>     {ipmonitor, Monitor, "120.0.0.1", false} ->
    3>         false
    3> end.

The options for ipmonitor:monitor/2 are:

* `timeout` - ipmonitor checks IP address (update its presence status) each `timeout` milliseconds.
  Default value - 3000.

IP address data type
--------------------

ipmonitor:ip_address() :: inet:ip_address() | binary() | string().

    1> application:start(ipmonitor).
    ok
    2> ipmonitor:monitor({127, 0, 0, 1}).
    {ok,#Ref<0.2891181753.4037017601.100471>,true}
    3> ipmonitor:monitor("127.0.0.1").   
    {ok,#Ref<0.2891181753.4037017601.100494>,true}
    4> ipmonitor:monitor(<<"127.0.0.1">>).
    {ok,#Ref<0.2891181753.4037017601.100514>,true}
