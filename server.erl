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
% When starting, it spawn in each node two processes to create files containig
% important informations that will be useful to the clients: the PID of the
% process of the server and the host name of each machine.
%
% Usage: server:start().
% To use the clients, the server must be on.
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

    % Read nodes id from enodes.conf
    {ok, [_|Ns]} = file:consult('enodes.conf'),
    % Send its own pid to evrery other pc on teda, so they can save it on a file
    [spawn(N, server, printPID, [self()]) || N <- Ns],
    % Tell each machine to print its own IP address
    [spawn(N, server, printIP, [N]) || N <- Ns],

    % Start interface
    spawn(server,interface,[self()]),

    % Launch server
    server([]).

% Core of the program
% List: [{PID,Name}] where
% PID: PID of the process sharing the file named Name
% Name: Name of the file avaible in the network
server(List) ->
    receive

        %-------------------------------------
        % Messages from clients
        %-------------------------------------
        
        % A new clients is connecting, we add its PID in the list
        {open, PID} ->
            PID ! connectionAccepted,
            List2 = List++[{PID,nofiles}],
            server(List2);

        % A new clients disconnects, we remove its PID and its files from the list
        {close, PID} ->
            List2 = [{P,Name} || {P,Name} <- List, P /= PID],
            server(List2);

        % A client just sent an updated list containig the names of files it is sharing
        {sharing, PID, SharedFiles} ->
            % Updated informations
            ListFiles = [{PID,Name} || Name <- SharedFiles],
            case ListFiles of
                [] ->
                    % If no file is listed, we remove the previous files...
                    ListMinusPID = [{P,Name} || {P,Name} <- List, P /= PID],
                    % ... and we note that this PID has no files
                    List2 = ListMinusPID++[{PID,nofiles}];
                _Else ->
                    % Otherwise we remove the previous files...
                    ListMinusPID = [{P,Name} || {P,Name} <- List, P /= PID],
                    % ... and we add the updated list
                    List2 = ListMinusPID++ListFiles
            end,
            server(List2);
    
        % A client requests the list of available files
        {showFilesRequest, PID} ->
            % Usort to get unique elements in the list
            SharedFiles = lists:usort([Name || {_,Name} <- List, Name /= nofiles]),
            % Sends information back to client
            PID ! {showFilesAnswer, SharedFiles},
            server(List);

        % A client wants to download a specific file, so the server sends it back
        % the list of PIDs sharing that file
        {downloadServer, PID, FileName} ->
            % Translate to binary the PIDs to send them with !
            Uploaders = [erlang:term_to_binary(P) || {P,Name} <- List, Name == FileName],
            case Uploaders of
                [] ->
                    % If the list is empty, the client receive the atom nothing
                    PID ! {downloadUploaders, nothing};
                _Else ->
                    % Sends the list of uploaders
                    PID ! {downloadUploaders, Uploaders}
            end,
            server(List);

        %-------------------------------------
        % Messages from interface
        %-------------------------------------

        % Show the list of available files
        % We read and sort from the Name of files the list and if there are disponibles files we print it
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

        
        % Show all the clients (process) connected 
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
% Interface
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

%-------------------------------------
% Printer
%-------------------------------------

% Print PID of server on a file.
% Useful for communicating with server, since they need to know the PID.
% PID: PID of server
printPID(PID) ->
    os:cmd("mkdir -p useful_files"),
    file:write_file("useful_files/serverPID", io_lib:format("~p.", [erlang:term_to_binary(PID)])).

% Print IP of a machine on a file.
% Useful to send files between client using TCP, since they need to know host name.
% Node: node of the machine itself
printIP(Node) ->
    {ok,[{IP,_,_}|_]} = rpc:call(Node,inet,getif,[]), % get server IP
    os:cmd("mkdir -p useful_files"),
    file:write_file("useful_files/machineIP", io_lib:format("~p.", [erlang:term_to_binary(IP)])).



