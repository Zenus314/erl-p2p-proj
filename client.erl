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
% CLIENT
% 
%
%
% Usage: client:start().
%
%=====================================

-module(client).
-export([start/0, client/2, client/1, interface/1]).

% Start the client
start() ->
    io:format("Client on~n"),

    % Read server PID
    {ok, [BinServerPID]} = file:consult('server_files/serverPID'),
    ServerPID = erlang:binary_to_term(BinServerPID),

    % Start interface
    spawn(client,interface,[self()]),

    client(ServerPID, connect).

% Connect client to server
client(ServerPID, connect) ->
    ServerPID ! {open, self()},
    % wait for reponse
    receive
        connectionAccepted ->
            io:format("Connected to server~n")
    end,
    client(ServerPID).

% Main program for client
client(ServerPID) ->
    % Check which files are avaible on the machine
    {ok, SharedFiles} = file:list_dir(shared_files),

    receive

        % Messages from interface
        showUpload ->
            case SharedFiles of
                [] ->
                    io:format("No files for upload~n");
                _Else ->
                    io:format("File for upload~n"),
                    [io:format("~s~n", [FileName]) || FileName <- SharedFiles]    
            end,
            spawn(client,interface,[self()]),
            client(ServerPID);

        stop ->
            ServerPID ! {close, self()}
    end.


%-------------------------------------
% Utilities
%-------------------------------------
% Process to give commands to the client
% PID: pid of the main process
interface(PID) ->
    io:format("~nEnter command~n"),
    io:format("u: show upload files~n"),
    io:format("q: close program~n"),
    Input = io:fread("","~s"),
    case Input of
        {ok,["u"]} -> PID ! showUpload;
        {ok,["q"]} -> PID ! stop;
        _Else -> io:format("Unrecognized input~n",[]), interface(PID)
    end.
