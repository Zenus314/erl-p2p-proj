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
-export([start/0, server/1, interface/1]).


%-------------------------------------
% Main program
%-------------------------------------
% Start the server
start() ->
    io:format("Server on~n"),

    % Send its own pid to evrery other pc on teda, so they can save it on a file.
    {ok, [_|Ns]} = file:consult('enodes.conf'),  %--- read nodes id from enodes.conf
    [spawn(N, printer, printPID, [self()]) || N <- Ns],

    % Start interface
    spawn(server,interface,[self()]),

    % Launch server
    server([]).

% Core of the program
% Ns: list of connected processes
server(PIDs) ->
    receive

        % Messages from clients
        {open, PID} ->
            %io:format("Client connected, PID:~p~n",[PID]),
            PIDs2 = PIDs++[PID],
            server(PIDs2);

        {close, PID} ->
            %io:format("Client disconnected, PID:~p~n",[PID]),
            PIDs2 = [N || N <- PIDs, N /= PID],
            server(PIDs2);

        % Messages from interface
        showProcesses ->
            case PIDs of
            [] ->
                io:format("No connected processes.~n");
            _Else ->
                io:format("Connected processes:~n"),
                [io:format("~p~n",[N]) || N <- PIDs]
            end,
            interface(self()),
            server(PIDs);

        stop ->
            io:format("Server closing~n"),
            stop
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
    {ok,[C]} = io:fread("","~s"),
    case C of
        "p" -> PID ! showProcesses;
        "q" -> PID ! stop;
        _Else -> io:format("Unrecognized input~n",[]), interface(PID)
    end.
