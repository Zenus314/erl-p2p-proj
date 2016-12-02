# erl-p2p-proj

Usiamo questa repository solo per condividere i tre (per ora) file di codice.

Per usarli, occorre salvarli nell'ambiente teda, sotto la cartella seguente:
~/mpe/erl/teda/apps/p2p/

28.11.2016:
- I client possono connettersi al server.

Prossimo passo:
- I client devono saper riconoscere quali file hanno a disposizione nella cartella apposita e inviare l'informazione al server.
[La cartella potrebbe essere .../p2p/shared_files e il client deve ottenere i nomi dei file come stringhe.]
- Il server deve avere a disposizione una struttura dati che permette di sapere quali file sono disponibili nella rete e da quali macchine (o meglio, processi).
[Immagino una cosa tipo una lista di tuple (nome_file,[lista dei pid che condividono il file]).]

29.11.2016
- Il server sa quali file sono disponibili nella rete.

Prossimo passo:
- Il client deve sapere quali file sono disponibili nella rete e poter decidere quale scaricare.
[Includere una tabella che mostra se il client possiede già il file o meno.]
- Due client devono potersi inviare un file intero tra di loro.
[Testare su un programma a parte.]

2.12.2016 (Mattina)
- I client possono scaricare un file scrivendone il nome. I file condivisi sono contenuti nella cartella "~/mpe/erl/teda/p2p_shared_files".

Prossimo passo:
- Correggere il problema delle porte: un client può scaricare un solo file alla volta. 
Probabile causa: la porta non viene chiusa correttamente al termine dello scambio, restando quindi inutilizzabile per un altro scambio.
Possibili soluzioni: chiudere nel modo corretto la porta, oppure svolgere le operazioni con un processo a parte, che quando termina chiude automaticamente le porte (forse).
