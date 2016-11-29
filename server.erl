%=====================================
% Matteo Badaracco
% Zeno Bardelli
%-------------------------------------
% Concurrent and Distributed Computing
% SA 2016 Project
% Université de Fribourg
%-------------------------------------
% Peer-to-peer
%=====================================
%
% SERVER
% This program run on a specific computer to act as server of our p2p program.
% The purpose of the server is to share informations between clients about
% files avaiability and locations.
% It creates and delete 3 kind of files:
% - server_pid: a textg file containing the 
% - list_of_files: a text containing a list of all the shared files at the moment,
% - <file>.txt: a text containing the nodes of each computer sharing <file>.
%
% Usage: server:start().
%
%=====================================

-module(server).
-export([start/0, server/1, interface/1, printPID/1]).


%-------------------------------------
% Main program
%-------------------------------------
% Start the server
start() ->
    io:format("Server on~n"),

    % Send its own pid to evrery other pc on teda, so they can save it on a file.
    {ok, [_|Ns]} = file:consult('enodes.conf'),  %--- read nodes id from enodes.conf
    [spawn(N, server, printPID, [self()]) || N <- Ns],

    % Start interface
    spawn(server,interface,[self()]),

    % Launch server
    server([]).

% Core of the program
% List: [(PID,[(Name,[Parts])])] where
% PID: PID of the process sharing N
% Name: Name of the file avaible in the network
% [Parts]: Parts of the file avaible from PID (nth is 1 if the PID can share the
%          nth part, for now is only one 1). 
server(List) ->
    receive

        % Messages from clients
        {open, PID} ->
            PID ! connectionAccepted,
            List2 = List++[{PID,[]}],
            server(List2);

        {close, PID} ->
            List2 = [{P,Rest} || {P,Rest} <- List, P /= PID],
            server(List2);

        % Messages from interface
        showProcesses ->
            case List of
            [] ->
                io:format("No connected processes.~n");
            _Else ->
                io:format("Connected processes:~n"),
                [io:format("~p~n",[P]) || {P,_} <- List]
            end,
            spawn(server,interface,[self()]),
            server(List);

        stop ->
            io:format("Server closing~n")
    end.

%-------------------------------------
% Utilities
%-------------------------------------
% Process to give commands to the server
% PID: pid of the main process
interface(PID) ->
    io:format("~nEnter command:~n"),
    io:format("p: show connected processes~n"),
    io:format("q: close server~n"),
    Input = io:fread("","~s"),
    case Input of
        {ok,["p"]} -> PID ! showProcesses;
        {ok,["q"]} -> PID ! stop;
        _Else -> io:format("Unrecognized input~n",[]), interface(PID)
    end.

% Print PID of server on a file
printPID(PID) ->
    file:write_file("server_files/serverPID", io_lib:format("~p.", [erlang:term_to_binary(PID)])).
