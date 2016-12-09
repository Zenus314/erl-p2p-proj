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
% files availability and locations.
% It creates and delete 3 kind of files:
% - server_pid: a textg file containing the 
% - list_of_files: a text containing a list of all the shared files at the moment,
% - <file>.txt: a text containing the nodes of each computer sharing <file>.
%
% Usage: server:start().
%
%=====================================

-module(server).
-export([start/0, server/1, interface/1, printPID/1, printIP/1]).


%-------------------------------------
% Main program
%-------------------------------------
% Start the server
start() ->
    io:format("Server on~n"),

    % Send its own pid to evrery other pc on teda, so they can save it on a file.
    {ok, [_|Ns]} = file:consult('enodes.conf'),  %--- read nodes id from enodes.conf
    [spawn(N, server, printPID, [self()]) || N <- Ns],
    % Tell each machine to print its own IP address
    [spawn(N, server, printIP, [N]) || N <- Ns],

    % Start interface
    spawn(server,interface,[self()]),

    % Launch server
    server([]).

% Core of the program
% List: [{PID,Name}] where
% PID: PID of the process sharing N
% Name: Name of the file avaible in the network
server(List) ->
    receive

        % Messages from clients
        {open, PID} ->
            PID ! connectionAccepted,
            List2 = List++[{PID,nofiles}],
            server(List2);

        {close, PID} ->
            List2 = [{P,Name} || {P,Name} <- List, P /= PID],
            server(List2);

        {sharing, PID, SharedFiles} ->
            % Update informations
            ListFiles = [{PID,Name} || Name <- SharedFiles],
            case ListFiles of
                [] ->
                    ListMinusPID = [{P,Name} || {P,Name} <- List, P /= PID],
                    List2 = ListMinusPID++[{PID,nofiles}];
                _Else ->
                    ListMinusPID = [{P,Name} || {P,Name} <- List, P /= PID],
                    List2 = ListMinusPID++ListFiles
            end,
            server(List2);

        {showFilesRequest, PID} ->
            SharedFiles = lists:usort([Name || {_,Name} <- List, Name /= nofiles]),
            PID ! {showFilesAnswer, SharedFiles},
            server(List);

        {downloadServer, PID, FileName} ->
            % Translate to binary to send with !
            Uploaders = [erlang:term_to_binary(P) || {P,Name} <- List, Name == FileName],
            case Uploaders of
                [] ->
                    PID ! {downloadUploaders, nothing},
                    server(List);
                _Else ->
                    PID ! {downloadUploaders, Uploaders},
                    ListMinusPID = [{P,Name} || {P,Name} <- List, P /= PID],
                    List2 = ListMinusPID++[{PID,nofiles}],
                    server(List2)
            end;

        % Messages from interface
        showFiles ->
            SharedFiles = lists:usort([Name || {_,Name} <- List, Name /= nofiles]),
            case SharedFiles of
                [] ->
                    io:format("No available files.~n");
                _Else ->
                    io:format("Available files:~n"),
                    [io:format("~s~n",[F]) || F <- SharedFiles]
            end,
            spawn(server,interface,[self()]),
            server(List);

        showProcesses ->
            case List of
            [] ->
                io:format("No connected processes.~n");
            _Else ->
                io:format("Connected processes:~n"),
                ListUnique = lists:usort([P || {P,_} <- List]),
                [io:format("~p~n",[P]) || P <- ListUnique]
            end,
            spawn(server,interface,[self()]),
            server(List);

        stop ->
            io:format("Server closing~n")
    end.

%-------------------------------------
% Utilities
%-------------------------------------
% Process to give commands to the server.
% PID: pid of the main process
interface(PID) ->
    io:format("~nEnter command:~n"),
    io:format("f: show available files~n"),
    io:format("p: show connected processes~n"),
    io:format("q: close server~n"),
    Input = io:fread("","~s"),
    case Input of
        {ok,["f"]} -> PID ! showFiles;
        {ok,["p"]} -> PID ! showProcesses;
        {ok,["q"]} -> PID ! stop;
        _Else -> io:format("Unrecognized input~n",[]), interface(PID)
    end.

% Print PID of server on a file.
% Useful for communicating with server.
% PID: PID of server
printPID(PID) ->
    file:write_file("useful_files/serverPID", io_lib:format("~p.", [erlang:term_to_binary(PID)])).

% Print IP of a machine on a file.
% Useful to send files between client using TCP.
% Node: node of the machine itself
printIP(Node) ->
    {ok,[{IP,_,_}|_]} = rpc:call(Node,inet,getif,[]), % get server IP
    file:write_file("useful_files/machineIP", io_lib:format("~p.", [erlang:term_to_binary(IP)])).
