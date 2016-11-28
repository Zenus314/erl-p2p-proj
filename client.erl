%=====================================
% Matteo Badaracco
% Zeno Bardelli
%-------------------------------------
% Concurrent and Distributed Computing
% SA 2016 Project
%-------------------------------------
% Peer-to-peer
%=====================================
%
% CLIENT
% 
%
%
% Usage: client:start().
%
%=====================================

-module(client).
-export([start/0, client/1]).

start() ->
    % Read server PID
    {ok, [BinServerPID]} = file:consult('server_files/serverPID'),
    ServerPID = erlang:binary_to_term(BinServerPID),
    client(ServerPID).

client(ServerPID) ->
    ServerPID ! {open, self()},
    {ok,[C]} = io:fread("Enter a digit to end","~s"),
    ServerPID ! {close, self()}.

