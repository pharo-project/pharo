public protocol
localHostName: aString
	"The local host name for purposes of identifying the the server.
 	If nil, NetNameResolver localHostName will be used."
 
 	^self connectionInfo at: #localHostName put: aString