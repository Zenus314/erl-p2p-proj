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
% Usage: client:start().
%
% Save the shared files in the folder: ../../p2p_shared_files/
%
%=====================================

-module(client).
-export([start/0, client/2, interface/1,file_name_receiver/4, file_receiver_loop/6, save_file/4, send_file/4, downloadPreparation/7, waitDownloadEnd/1]).


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
    
    % Create p2p_shared_files if does not exist
    os:cmd("mkdir -p ../../p2p_shared_files"),

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

    % Prepare useful script for execution
    os:cmd("chmod +x portsFinder.sh"),

    % Start program
    client(ServerPID, MachineIP).

% Main program for client
client(ServerPID, MachineIP) ->
    % Check which files are available on the machine
    {ok, SharedFiles} = file:list_dir('../../p2p_shared_files'),

    % Send the information to the server
    ServerPID ! {sharing, self(), SharedFiles},

    receive
        %-------------------------------------
        % Messages from other clients
        %-------------------------------------

        % A client ask to download a file
        % We create a temporary directory and we split the request file in N pieces
        % In order distinguish every download we named the folder with the file name and the seconds of when the download start compute from the 1970
        % N is given from the requesting client and is equivalent to the number of seeder of the file
        % FileNumber: the number of the piece that this client must send
        {downloadRequest, Host, FileName, Port, FileNumber, NbU, DownloadID} ->
            os:cmd("mkdir -p tmp_"++FileName++DownloadID),
            FilePath = "tmp_"++FileName++DownloadID++"/",

            os:cmd("split -n "++io_lib:format("~p ",[NbU])++
                   "../../p2p_shared_files/"++FileName++" "++FilePath),
            Files = os:cmd("ls "++FilePath),
            FilesList = string:tokens(Files,"\n"),
            % Find the name of the file that he must send
            FilePiece = lists:nth(FileNumber,FilesList),
            spawn(client,send_file,[Host,FilePiece,FilePath,Port]),
            client(ServerPID, MachineIP);

        % The client has complete the download and then we can remove the temporary folder
		{thanks, FileName, DownloadID} ->
            os:cmd("rm -r tmp_"++FileName++DownloadID),
            client(ServerPID, MachineIP);
        
        %-------------------------------------
        % Messages from interface
        %-------------------------------------
       
        % Start the download
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
                        
                         % We receive the list of Uploaders
                        {downloadUploaders, BinUploaders} -> 
                            % First convert the binary form
                            Uploaders = [erlang:binary_to_term(P) || P <- BinUploaders],
                            NbU = length(Uploaders),
                     
                            % Choose a free port (we use a script for that)
                            String = os:cmd("./portsFinder.sh "++io_lib:format("~p",[NbU])),
                            StringPorts = string:tokens(String,"\n"),
                            Ports = [Q || {Q,_} <- [string:to_integer(P) || P <- StringPorts]],
                            
                            % Creare string to uniquely identify download
                            % Seconds an nanosecond from the 1970
                            [DownloadID] = string:tokens(os:cmd("date +%s%N"),"\n"),

                            % Make a new temporary directory where we will save all the files' pieces
                            os:cmd("mkdir -p tmp_"++FileName++DownloadID),

                            % Start the counter in order to know the download time
                            StartTime = now(),
                            % Send request to other clients
                            downloadPreparation(Ports,Uploaders,1,NbU,MachineIP,FileName,DownloadID),
                           
                            % Wait the end of the download of each piece
                            waitDownloadEnd(NbU),

                            % The download is ended we must merge all the pieces and save the final file to ../../p2p_shared_files/
                            io:format("Merging file fragments...~n"),
                            os:cmd("cat tmp_"++FileName++DownloadID++"/* > ../../p2p_shared_files/"++FileName),
                            % Timer stopped
                            EndTime = now(),
                            DownloadTime = timer:now_diff(EndTime,StartTime) / 1000000,
                            io:format("Download Time: ~ps~n",[DownloadTime]),
                            % Delete temporary folder and we tell to the uploader that we have finished the download
                            os:cmd("rm -r tmp_"++FileName++DownloadID),
                            [U ! {thanks,FileName, DownloadID} || U <- Uploaders]
                    end
            end,
            % Reboot the interface and the client
            spawn(client,interface,[self()]),
            client(ServerPID, MachineIP);

        % Show the IP of the machine
        showIP ->
            io:format("The IP of this machine is: "),
            io:format("~p~n",[MachineIP]),
            spawn(client,interface,[self()]),
            client(ServerPID, MachineIP);

        % Show all the avaiable files that all the clients connected are sharing
        showFiles ->
            % Ask to the server the list of files
            ServerPID ! {showFilesRequest, self()},
            receive
                % Print the answer
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
         
        % Show the files that we own and that the others could download
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
        
        % The client shutdown and tell to the server that he is shutting down
        stop ->
            ServerPID ! {close, self()}
    end.


%-------------------------------------
% Download Functions
%-------------------------------------

% Send to the uploaders the right port and the right number of the pieces to sened
% It's a loop that every time take the first free port and send all to the first uploader
% Then he spawn a new process in order to receive that piece of file
downloadPreparation([Port|Ports],[Uploader|Uploaders],FileNumber, NbU, Host, FileName, DownloadID) ->
    % Send request to others
    Uploader ! {downloadRequest, Host, FileName, Port, FileNumber, NbU, DownloadID},
    % Prepare to receive a socket
    spawn(client,file_name_receiver,[FileName,Port,self(),DownloadID]),
    % If there is still a piece to send we start again the loop else end
    case FileNumber of
        NbU -> ok;
        _Else -> downloadPreparation(Ports,Uploaders,FileNumber+1, NbU, Host, FileName, DownloadID)
    end.

% Wait that all the pieces of the file are downloaded
waitDownloadEnd(NbU) ->
    case NbU of
        0 -> ok;
        _Else -> 
            receive
                downloadFinished -> waitDownloadEnd(NbU-1)
            end
    end.

%-------------------------------------
% TCP file sharing
%-------------------------------------
% Client want to receive file
% Port: used port
% PID: PID of the client porcess, useful to tell when download ended
file_name_receiver(OriginalFileName,Port,PID,DownloadID)->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    {ok, Socket} = gen_tcp:accept(LSock),
    {ok,FileNameBinaryPadding}=gen_tcp:recv(Socket,30),
    FileNamePadding=erlang:binary_to_list(FileNameBinaryPadding),
    FileName = string:strip(FileNamePadding,both,$ ),
    io:format("~nReceiving file~n"),
    file_receiver_loop(Socket,OriginalFileName,FileName,[],PID,DownloadID).

% Loop to get the whole file
% Socket: sending sockets
% FileName: name of the chunk of the original file
% Bs: received binaries
% PID: PID of the client porcess, useful to tell when download ended
% DownloadID needed to find the path to the download folder
file_receiver_loop(Socket,OriginalFileName,FileName,Bs,PID,DownloadID)->
    case gen_tcp:recv(Socket, 0) of
    % File received wait another file
    {ok, B} ->
        file_receiver_loop(Socket,OriginalFileName,FileName,[Bs, B],PID,DownloadID);
    % An error occour or we have all the pieces
    % we save the file and we end the connection
    {error, closed} ->
        save_file(OriginalFileName,FileName,Bs,DownloadID),
        ok = gen_tcp:close(Socket),
        PID ! downloadFinished
    end.

% Save the file in the temporary folder
% FileName: name of the file
% Bs: received file
save_file(OriginalFileName,FileName,Bs,DownloadID) ->
    io:format("~nFilename: ~p~nDownload complete~n",[FileName]),
    FilePath = "tmp_"++OriginalFileName++DownloadID++"/",
    {ok, Fd} = file:open(FilePath++FileName, write),
    file:write(Fd, Bs),
    file:close(Fd).

% Client which send a file
% Host: IP address of target
% FileName: name of file
% FilePath: path to file (FileName excluded)
% Port: used port
send_file(Host,FileName,FilePath,Port)->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    FileNamePadding = string:left(FileName, 30, $ ), % Padding with white space
    gen_tcp:send(Socket,FileNamePadding),
    file:sendfile(FilePath++FileName, Socket),
    ok = gen_tcp:close(Socket).


%-------------------------------------
% Utilities
%-------------------------------------
% Process to give commands to the client
% PID: pid of the main process
interface(PID) ->
    io:format("~nEnter command~n"),
    io:format("d: go to download~n"),
    io:format("a: show ip address~n"),    
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


