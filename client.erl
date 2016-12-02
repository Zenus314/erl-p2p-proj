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
-export([start/0, client/2, interface/1,file_name_receiver/2, file_receiver_loop/4, save_file/2, send_file/4]).


%-------------------------------------
% Main program
%-------------------------------------
% Start the client
start() ->
    io:format("Client on~n"),

    % Read server PID
    {ok, [BinServerPID]} = file:consult('useful_files/serverPID'),
    ServerPID = erlang:binary_to_term(BinServerPID),
    % Read machine IP
    {ok, [BinMachineIP]} = file:consult('useful_files/machineIP'),
    MachineIP = erlang:binary_to_term(BinMachineIP),
    
    % Connect client to server
    io:format("Connecting to server...~n"),
    ServerPID ! {open, self()},
    % wait for reponse
    receive
        connectionAccepted ->
            io:format("Connected to server.~n")
    end,

    % Start interface
    spawn(client,interface,[self()]),
    % Start program
    client(ServerPID, MachineIP).

% Main program for client
client(ServerPID, MachineIP) ->
    % Check which files are available on the machine
    {ok, SharedFiles} = file:list_dir('../../p2p_shared_files'),

    % Send the information to the server
    ServerPID ! {sharing, self(), SharedFiles},

    receive
        % Messages from other clients
        {downloadRequest, Host, FileName, Port} ->
            FilePath = "../../p2p_shared_files/"++FileName,
            spawn(client,send_file,[Host,FileName,FilePath,Port]),
            client(ServerPID, MachineIP);

        % Messages from interface
        {downloadStart, FileName} ->
            OwnFile = lists:member(FileName, SharedFiles),
            % Check if it already has the file
            case OwnFile of
                true ->
                    io:format("File ~p is already on this pc~n",[FileName]);
                _Else ->
                    % Request informations to server
                    ServerPID ! {downloadServer, self(), FileName},
                    receive
                        {downloadUploaders, nothing} ->
                            io:format("No uploader for ~p.~n", [FileName]);

                        {downloadUploaders, BinUploaders} -> 
                            % First convert the binary form
                            Uploaders = [erlang:binary_to_term(P) || P <- BinUploaders],
                            % For now we simply take the first PID in the list
                            [OtherPID|_] = Uploaders,
                            % Choose a free (at least it is my hope) port
                            Port = 5678,
                            % Send request to other client
                            OtherPID ! {downloadRequest, MachineIP, FileName, Port},
                            % Wait and download file
                            spawn(client,file_name_receiver,[Port,self()]),
                            % Wait the end of download to restart client
                            receive downloadFinished -> ok end
                    end
            end,
            spawn(client,interface,[self()]),
            client(ServerPID, MachineIP);

        showIP ->
            io:format("The IP of this machine is: "),
            io:format("~p~n",[MachineIP]),
            spawn(client,interface,[self()]),
            client(ServerPID, MachineIP);

        showFiles ->
            ServerPID ! {showFilesRequest, self()},
            receive
                {showFilesAnswer, GlobalSharedFiles} ->
                    case GlobalSharedFiles of
                        [] ->
                            io:format("No available files.~n");
                        _Else ->
                            GlobalSharedFilesNotOwn = [F || F <- GlobalSharedFiles, not(lists:member(F,SharedFiles))],
                            io:format("Available files:~n"),
                            [io:format("~s~s~n",["not own-- ",F]) || F <- GlobalSharedFilesNotOwn],
                            [io:format("~s~s~n",["own    -- ",F]) || F <- SharedFiles]
                    end
            end,
            spawn(client,interface,[self()]),
            client(ServerPID, MachineIP);

        showUpload ->
            case SharedFiles of
                [] ->
                    io:format("No files for upload.~n");
                _Else ->
                    io:format("File for upload:~n"),
                    [io:format("~s~n", [FileName]) || FileName <- SharedFiles]    
            end,
            spawn(client,interface,[self()]),
            client(ServerPID, MachineIP);

        stop ->
            ServerPID ! {close, self()}
    end.



%-------------------------------------
% TCP file sharing
%-------------------------------------
% Client want to receive file
% Port: used port
file_name_receiver(Port,PID)->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    {ok, Socket} = gen_tcp:accept(LSock),
    {ok,FileNameBinaryPadding}=gen_tcp:recv(Socket,30),
    FileNamePadding=erlang:binary_to_list(FileNameBinaryPadding),
    FileName = string:strip(FileNamePadding,both,$ ),
    file_receiver_loop(Socket,FileName,[],PID).

% Loop to get the whole file
% Socket: sending sockets
% FileName: name of the file
% Bs: received binaries
file_receiver_loop(Socket,FileName,Bs,PID)->
    io:format("~nReceiving file~n"),
    case gen_tcp:recv(Socket, 0) of
    {ok, B} ->
        file_receiver_loop(Socket, FileName,[Bs, B],PID);
    {error, closed} ->
        save_file(FileName,Bs),
        ok = gen_tcp:close(Socket),
        PID ! downloadFinished
    end.

% Save the file in the right position (../../p2p_shared_files)
% FileName: name of the file
% Bs: received file
save_file(FileName,Bs) ->
    io:format("~nFilename: ~p~nDownload complete~n",[FileName]),
    FilePath = "../../p2p_shared_files/",
    {ok, Fd} = file:open(FilePath++FileName, write),
    file:write(Fd, Bs),
    file:close(Fd).

% Client which send a file
% Host: IP address of target
% FileName: name of file
% FilePath: path to file (FileName included)
% Port: used port
send_file(Host,FileName,FilePath,Port)->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    FileNamePadding = string:left(FileName, 30, $ ), % Padding with white space
    gen_tcp:send(Socket,FileNamePadding),
    file:sendfile(FilePath, Socket),
    ok = gen_tcp:close(Socket).


%-------------------------------------
% Utilities
%-------------------------------------
% Process to give commands to the client
% PID: pid of the main process
interface(PID) ->
    io:format("~nEnter command~n"),
    io:format("d: go to download~n"),
    io:format("a: show ip adress~n"),    
    io:format("f: show available files~n"),
    io:format("u: show upload files~n"),
    io:format("q: close program~n"),
    Input = io:fread("","~s"),
    case Input of
        {ok,["d"]} -> interface_download(PID);
        {ok,["a"]} -> PID ! showIP;
        {ok,["f"]} -> PID ! showFiles;
        {ok,["u"]} -> PID ! showUpload;
        {ok,["q"]} -> PID ! stop;
        _Else -> io:format("Unrecognized input~n",[]), interface(PID)
    end.

interface_download(PID) ->
    io:format("~nEnter the name of the file you want to download or b to go back.~n"),
    Input = io:fread("","~s"),
    case Input of
        {ok, ["b"]} -> interface(PID);
        {ok, [FileName]} -> PID ! {downloadStart, FileName};
        _Else -> io:format("Unrecognized input~n",[]), interface_download(PID)
    end.

