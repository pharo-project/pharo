"
ProtocolClient is the abstract super class for a variety of network protocol clients.
It uses a stream rather than the direct network access so it could also work for streams on serial connections etc.

Structure:
	stream				stream representing the connection to and from the server
	connectInfo			information required for opening a connection
	lastResponse			remembers the last response from the server.
	progressObservers 	any object understanding #show: can be registered as a progress observer (login, transfer, etc)
"
Class {
	#name : #ProtocolClient,
	#superclass : #Object,
	#instVars : [
		'stream',
		'connectInfo',
		'lastResponse',
		'pendingResponses',
		'progressObservers'
	],
	#category : #'Network-Protocols-Protocols'
}

{ #category : #accessing }
ProtocolClient class >> defaultPortNumber [
	self subclassResponsibility
]

{ #category : #accessing }
ProtocolClient class >> logFlag [
	self subclassResponsibility
]

{ #category : #'instance creation' }
ProtocolClient class >> openOnHost: hostIP port: portNumber [
	^self new openOnHost: hostIP port: portNumber
]

{ #category : #'instance creation' }
ProtocolClient class >> openOnHostNamed: hostName [
	"If the hostname uses the colon syntax to express a certain portnumber
	we use that instead of the default port number."

	| i |
	i := hostName indexOf: $:.
	i = 0 ifTrue: [
			^self openOnHostNamed: hostName port: self defaultPortNumber]
		ifFalse: [
			| s p | 
			s := hostName truncateTo: i - 1.
			p := (hostName copyFrom: i + 1 to: hostName size) asInteger.
			^self openOnHostNamed: s port: p]
	
]

{ #category : #'instance creation' }
ProtocolClient class >> openOnHostNamed: hostName port: portNumber [
	| serverIP |
	serverIP := NetNameResolver addressForName: hostName timeout: 20.
	^self openOnHost: serverIP port: portNumber

]

{ #category : #'private protocol' }
ProtocolClient >> checkForPendingError [
	"If data is waiting, check it to catch any error reports.
	In case the response is not an error, push it back."

	self stream isDataAvailable
		ifFalse: [^self].
	self fetchNextResponse.
	self
		checkResponse: self lastResponse
		onError: [:response | (TelnetProtocolError protocolInstance: self) signal]
		onWarning: [:response | (TelnetProtocolError protocolInstance: self) signal].
	"if we get here, it wasn't an error"
	self pushResponse: self lastResponse
]

{ #category : #'private protocol' }
ProtocolClient >> checkResponse [
	self
		checkResponseOnError: [:response | (TelnetProtocolError protocolInstance: self)
				signal: response]
		onWarning: [:response | (TelnetProtocolError protocolInstance: self)
				signal: response]
]

{ #category : #'private protocol' }
ProtocolClient >> checkResponse: aResponse onError: errorBlock onWarning: warningBlock [
	"Get the response from the server and check for errors. Invoke one of the blocks if an error or warning is encountered. See class comment for classification of error codes."

	self responseIsError
		ifTrue: [errorBlock value: aResponse].
	self responseIsWarning
		ifTrue: [warningBlock value: aResponse].

]

{ #category : #'private protocol' }
ProtocolClient >> checkResponseOnError: errorBlock onWarning: warningBlock [
	"Get the response from the server and check for errors. Invoke one of the blocks if an error or warning is encountered. See class comment for classification of error codes."

	self fetchPendingResponse.
	self checkResponse: self lastResponse onError: errorBlock onWarning: warningBlock
]

{ #category : #actions }
ProtocolClient >> close [
	self stream
		ifNotNil: [
			self stream close.
			stream := nil]
]

{ #category : #private }
ProtocolClient >> connectionInfo [
	connectInfo ifNil: [connectInfo := Dictionary new].
	^connectInfo
]

{ #category : #private }
ProtocolClient >> defaultPortNumber [
	^self class defaultPortNumber
]

{ #category : #private }
ProtocolClient >> ensureConnection [
	self isConnected
		ifTrue: [^self].
	self stream
		ifNotNil: [self stream close].

	self stream: (SocketStream openConnectionToHost: self host port: self port).
	self checkResponse.
	self login
]

{ #category : #'private protocol' }
ProtocolClient >> fetchNextResponse [
	self lastResponse: self stream nextLine
]

{ #category : #'private protocol' }
ProtocolClient >> fetchPendingResponse [
	^pendingResponses
		ifNil: [self fetchNextResponse; lastResponse]
		ifNotNil: [self popResponse]
]

{ #category : #private }
ProtocolClient >> host [
	^self connectionInfo at: #host
]

{ #category : #private }
ProtocolClient >> host: hostId [
	^self connectionInfo at: #host put: hostId
]

{ #category : #testing }
ProtocolClient >> isConnected [
	^stream notNil
		and: [stream isConnected]
]

{ #category : #private }
ProtocolClient >> lastResponse [
	^lastResponse
]

{ #category : #private }
ProtocolClient >> lastResponse: aString [
	lastResponse := aString.

]

{ #category : #private }
ProtocolClient >> logFlag [
	^self class logFlag
]

{ #category : #private }
ProtocolClient >> logProgress: aString [
	self progressObservers do: [:each | each show: aString].

]

{ #category : #accessing }
ProtocolClient >> logProgressToTranscript [
	self progressObservers add: Transcript
]

{ #category : #accessing }
ProtocolClient >> messageText [
	^super messageText
		ifNil: [self response]
]

{ #category : #private }
ProtocolClient >> openOnHost: hostIP port: portNumber [
	self host: hostIP.
	self port: portNumber.
	self ensureConnection
]

{ #category : #private }
ProtocolClient >> password [
	^self connectionInfo at: #password
]

{ #category : #private }
ProtocolClient >> password: aString [
	^self connectionInfo at: #password put: aString
]

{ #category : #private }
ProtocolClient >> pendingResponses [
	pendingResponses ifNil: [pendingResponses := OrderedCollection new].
	^pendingResponses
]

{ #category : #private }
ProtocolClient >> popResponse [
	| pendingResponse |
	pendingResponse := self pendingResponses removeFirst.
	pendingResponses isEmpty
		ifTrue: [pendingResponses := nil].
	^pendingResponse
]

{ #category : #private }
ProtocolClient >> port [
	^self connectionInfo at: #port
]

{ #category : #private }
ProtocolClient >> port: aPortNumber [
	^self connectionInfo at: #port put: aPortNumber
]

{ #category : #private }
ProtocolClient >> progressObservers [
	progressObservers ifNil: [progressObservers := OrderedCollection new].
	^progressObservers
]

{ #category : #private }
ProtocolClient >> pushResponse: aResponse [
	self pendingResponses add: aResponse
]

{ #category : #actions }
ProtocolClient >> reopen [
	self ensureConnection
]

{ #category : #private }
ProtocolClient >> resetConnectionInfo [
	connectInfo := nil
]

{ #category : #accessing }
ProtocolClient >> response [
	^self protocolInstance lastResponse
]

{ #category : #'private testing' }
ProtocolClient >> responseIsError [
	self subclassResponsibility
]

{ #category : #'private testing' }
ProtocolClient >> responseIsWarning [
	self subclassResponsibility
]

{ #category : #'private protocol' }
ProtocolClient >> sendCommand: aString [
	self stream sendCommand: aString.

]

{ #category : #'private protocol' }
ProtocolClient >> sendStreamContents: aStream [
	self stream sendStreamContents: aStream
]

{ #category : #accessing }
ProtocolClient >> stream [
	^stream
]

{ #category : #accessing }
ProtocolClient >> stream: aStream [
	stream := aStream
]

{ #category : #private }
ProtocolClient >> user [
	^self connectionInfo at: #user ifAbsent: [nil]
]

{ #category : #private }
ProtocolClient >> user: aString [
	^self connectionInfo at: #user put: aString
]
