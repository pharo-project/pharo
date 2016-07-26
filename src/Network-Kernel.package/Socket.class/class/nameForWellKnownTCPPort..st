utilities
nameForWellKnownTCPPort: portNum
	"Answer the name for the given well-known TCP port number. Answer a string containing the port number if it isn't well-known."

	| portList |
	portList := #(#(7 'echo') #(9 'discard') #(13 'time') #(19 'characterGenerator') #(21 'ftp') #(23 'telnet') #(25 'smtp') #(80 'http') #(110 'pop3') #(119 'nntp')).
	^ portList
		detect: [ :pair | pair first = portNum ]
		ifFound: [ :pair | pair last ]
		ifNone: [ 'port-' , portNum printString ]