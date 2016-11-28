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
% PRINTER
% Print necessary files on a machine.
% - PID of the process server
%
% Usage: Used by other modules
%
%=====================================

-module(printer).
-export([printPID/1]).


%-------------------------------------
% Print PID of server on a file
%-------------------------------------
printPID(PID) ->
    file:write_file("server_files/serverPID", io_lib:format("~p.", [erlang:term_to_binary(PID)])).
