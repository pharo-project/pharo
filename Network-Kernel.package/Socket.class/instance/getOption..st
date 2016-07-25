other
getOption: aName 
	"Get options on this socket, see Unix man pages for values for 
	sockets, IP, TCP, UDP. IE SO_KEEPALIVE
	returns an array, element one is an status number (0 ok, -1 read only option)
	element two is the resulting of the requested option"

	(socketHandle == nil or: [self isValid not])
		ifTrue: [InvalidSocketStatusException signal: 'Socket status must valid before getting an option'].
	^self primSocket: socketHandle getOption: aName

"| foo options |
Socket initializeNetwork.
foo := Socket newTCP.
foo connectTo: (NetNameResolver addressFromString: '192.168.1.1') port: 80.
foo waitForConnectionUntil: (Socket standardDeadline).

options := {
'SO_DEBUG'. 'SO_REUSEADDR'. 'SO_REUSEPORT'. 'SO_DONTROUTE'.
'SO_BROADCAST'. 'SO_SNDBUF'. 'SO_RCVBUF'. 'SO_KEEPALIVE'.
'SO_OOBINLINE'. 'SO_PRIORITY'. 'SO_LINGER'. 'SO_RCVLOWAT'.
'SO_SNDLOWAT'. 'IP_TTL'. 'IP_HDRINCL'. 'IP_RCVOPTS'.
'IP_RCVDSTADDR'. 'IP_MULTICAST_IF'. 'IP_MULTICAST_TTL'.
'IP_MULTICAST_LOOP'. 'UDP_CHECKSUM'. 'TCP_MAXSEG'.
'TCP_NODELAY'. 'TCP_ABORT_THRESHOLD'. 'TCP_CONN_NOTIFY_THRESHOLD'. 
'TCP_CONN_ABORT_THRESHOLD'. 'TCP_NOTIFY_THRESHOLD'.
'TCP_URGENT_PTR_TYPE'}.

1 to: options size do: [:i | | fum |
	fum :=foo getOption: (options at: i).
	Transcript show: (options at: i),fum printString;cr].

foo := Socket newUDP.
foo setPeer: (NetNameResolver addressFromString: '192.168.1.9') port: 7.
foo waitForConnectionUntil: (Socket standardDeadline).

1 to: options size do: [:i | | fum |
	fum :=foo getOption: (options at: i).
	Transcript show: (options at: i),fum printString;cr].
"