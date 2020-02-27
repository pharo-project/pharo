"
I am ZnSingleThreadedServer.
I can be customized with a delegate (#handleRequest:) and an authenticator (#authenticateRequest:do:).

  ZnSingleThreadedServer startDefaultOn: 1701.
  ZnSingleThreadedServer default authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
  ZnClient new username: 'foo' password: 'secret'; get: 'http://localhost:1701'.

I use ZnDefaultServerDelegate when no other delegate is set.
I am single threaded, I run in a single process.
I close connections after each request/response cycle.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnSingleThreadedServer,
	#superclass : #ZnServer,
	#instVars : [
		'process',
		'serverSocket',
		'logLevel'
	],
	#classInstVars : [
		'default'
	],
	#category : #'Zinc-HTTP-Client-Server'
}

{ #category : #public }
ZnSingleThreadedServer class >> adoptAsDefault: server [
	"Adopt server as the default instance that we manage.
	If there was a previous default, stop it.
	Note that this is a class instance variable."
	
	self stopDefault.
	default := server
]

{ #category : #public }
ZnSingleThreadedServer class >> default [
	"Return the default instance that we manage.
	Note that this is a class instance variable."
	
	^ default
]

{ #category : #public }
ZnSingleThreadedServer class >> defaultOn: portNumber [
	"Return the default instance on a given port,
	Keep a reference to it in a class instance variable.
	If there was no previous default instance, create a new one.
	If there was a previous default instance, reuse it:
	if it was running stop it, change the port if necessary."

	^ default 
		ifNil: [
			default := self on: portNumber ] 
		ifNotNil: [ 
			default stop; port: portNumber; yourself ]
]

{ #category : #'class initialization' }
ZnSingleThreadedServer class >> initialize [
	default := nil
]

{ #category : #public }
ZnSingleThreadedServer class >> on: aNumber [
	"Instantiate and return a new listener on a given port,
	send #start to it to start listening."

	^ self new 
		port: aNumber; 
		yourself
]

{ #category : #public }
ZnSingleThreadedServer class >> startDefaultOn: portNumber [
	"Start and return the default instance on a given port.
	Keep a reference to it in a class instance variable.
	If there was no previous default instance, create a new one.
	If there was a previous default instance, reuse it:
	if it was running stop and start it, effectively restarting it.
	Change the port if necessary.
	Starting the default server will register it automatically."

	^ (self defaultOn: portNumber) 
		start; 
		yourself
]

{ #category : #public }
ZnSingleThreadedServer class >> startOn: portNumber [
	"Instanciate and return a new listener on a given port and start listening."

	^ (self on: portNumber) 
		start;
		yourself
]

{ #category : #public }
ZnSingleThreadedServer class >> stopDefault [
	"Stop and unregister the Default instance, if any, and clear it.
	Return the stopped instance, if any."
	
	| server |
	(server := default) ifNotNil: [ 
		default stop.  
		default := nil ].
	^ server
]

{ #category : #constants }
ZnSingleThreadedServer >> acceptWaitTimeout [
	"How many seconds to wait for a server socket listening for an accept ?"
	
	^ 300
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> augmentResponse: response forRequest: request [
	"Our handler has produced response for request, manipulate the response before writing it"
	
	(self useGzipCompressionAndChunking and: [ response contentType isBinary not ])
		ifTrue: [ 
			(request acceptsEncodingGzip and: [ response hasContentEncoding not ]) 
				ifTrue: [ response setContentEncodingGzip ].
			response hasTransferEncoding
				ifFalse: [ response setTransferEncodingChunked ] ]
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> authenticateAndDelegateRequest: request [
	"Handle request and return a response.
	If we have a delegate, pass the responsibility.
	If we have no delegate, we return a page not found.
	Make sure to pass via our authenticator."

	^ self
		authenticateRequest: request
		do: [ 
			self delegate
				ifNil: [ ZnResponse notFound: request uri ]
				ifNotNil: [ :delegate | delegate handleRequest: request ] ]
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> authenticateRequest: request do: block [
	"Validate request and execute block.
	When our authenticator is not nil, pass the responsibility"

	^ self authenticator
		ifNil: [ block value ]
		ifNotNil: [ :authenticator | authenticator authenticateRequest: request do: block ]
]

{ #category : #private }
ZnSingleThreadedServer >> closeDelegate [
	self delegate
		ifNotNil: [ :delegate | 
			(delegate respondsTo: #close)
				ifTrue: [ delegate close ] ]
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> executeOneRequestResponseOn: stream [
	"Execute one single HTTP request / response cycle on stream in 3 steps:
	#readRequest: #handleRequest: and #writeResponse:on:
	Indicate that we intend to close the connection afterwards"

	| request response timing|
	ZnCurrentServer
		value: self
		during: [ 
			timing := ZnServerTransactionTiming new.
			request := self readRequest: stream timing: timing.
			response := self handleRequest: request timing: timing.
			response setConnectionClose.
			self writeResponse: response on: stream timing: timing.
			self logServerTransactionRequest: request response: response timing: timing ]
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> handleRequest: request timing: timing [
	"Handle request and return a response.
	Do logging and timing. Set a dynamic variable referencing the server.
	If necessary do session management in the response."

	| response initialMilliseconds |
	initialMilliseconds := Time millisecondClockValue.
	ZnCurrentServerSession value: nil.
	response := self handleRequestProtected: request.
	request method = #HEAD
		ifTrue: [ response clearEntity ].
	timing handlerDuration: (self logRequest: request response: response handledStarted: initialMilliseconds).
	ZnCurrentServerSession value 
		ifNotNil: [ :session | self sessionManager setSession: session in: response ].
	^ response
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> handleRequestProtected: request [
	"Handle request and return a response.
	If a ZnRespond notification is signaled, return its response directly.
	If an Error is thrown, return a HTTP Server Error response."

	^ [ [ self authenticateAndDelegateRequest: request ]
			on: ZnRespond 
			do: [ :notification | notification response ] ]
		on: Error
		do: [ :exception | 
			self debugMode
				ifTrue: [ exception pass ]
				ifFalse: [
					self logServerHandlerError: exception.
					ZnResponse serverError: exception printString ] ]
]

{ #category : #initialization }
ZnSingleThreadedServer >> initialize [
	self loggingOn 
]

{ #category : #private }
ZnSingleThreadedServer >> initializeServerSocket [
	"Set up a new server socket and listen on it"
	
	serverSocket := self bindingAddress
		ifNil: [ ZnNetworkingUtils serverSocketOn: self port ]
		ifNotNil: [ ZnNetworkingUtils serverSocketOn: self port interface: self bindingAddress ].
	"Binding a server socket to port 0 means letting the OS assign a port, fetch it"
	self port isZero 
		ifTrue: [ self optionAt: #port put: serverSocket port ].
	self logServerSocketBound
]

{ #category : #testing }
ZnSingleThreadedServer >> isListening [
	"Return true when I have a valid server socket listening at the correct port"
	
	^ self serverSocket notNil
		and: [ self serverSocket isValid and: [ self serverSocket localPort = self port ] ]
]

{ #category : #testing }
ZnSingleThreadedServer >> isRunning [
	"Return true when I am running"
	
	^ self process notNil and: [ self serverSocket notNil ]
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> listenLoop [
	"We create a listening Socket, then wait for a connection.
	After each connection we also check that the listening Socket is still valid 
	- if not we just make a recursive call to this method to start over."

	self initializeServerSocket.
	[ [ 
		serverSocket isValid
			 ifFalse: [
				"will trigger #ifCurtailed: block and destroy socket"
				^ self listenLoop ].
		self serveConnectionOn: serverSocket ] repeat ]

		ifCurtailed: [ self releaseServerSocket ]
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logConnectionAccepted: socket [
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnConnectionAcceptedEvent)
		address: ([ socket remoteAddress ] on: Error do: [ nil ]);
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logConnectionClosed: socketStream [
	logLevel < 3 ifTrue: [ ^ nil ].
	socketStream socket ifNil: [ ^ nil ].
	^ (self newLogEvent: ZnServerConnectionClosedEvent)
		address: ([ socketStream socket remoteAddress] on: Error do: [ nil ]);
		emit
]

{ #category : #logging }
ZnSingleThreadedServer >> logLevel: integer [
	"Set the log level to integer.
	0 - no logging
	1 - simplified transaction logging
	2 - detailed transaction logging
	3 - log everything"
	
	logLevel := integer
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logRequest: request response: response handledStarted: initialMilliseconds [
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnRequestResponseHandledEvent)
				request: request;
				response: response;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logRequestRead: request started: initialMilliseconds [
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnRequestReadEvent)
				request: request;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logResponseWritten: response started: initialMilliseconds [
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnResponseWrittenEvent)
				response: response;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logServerGeneric: subject [
	^ (self newLogEvent: ZnServerGenericLogEvent)
		subject: subject;
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logServerHandlerError: exception [
	^ (self newLogEvent: ZnServerHandlerErrorEvent)
		exception: exception;
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logServerReadError: exception [
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerReadErrorEvent)
		exception: exception;
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logServerSocketBound [
	logLevel < 1 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerSocketBoundEvent)
		address: ([ serverSocket address ] on: Error do: [ nil ]);
		port: ([ serverSocket port ] on: Error do: [ nil ]);
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logServerSocketReleasedAddress: address port: port [
	logLevel < 1 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerSocketReleasedEvent)
		address: address;
		port: port;
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logServerTransactionRequest: request response: response timing: timing [
	| logEventClass |
	logLevel < 1 ifTrue: [ ^ nil ].
	logEventClass := logLevel = 1
		ifTrue: [ ZnSimplifiedServerTransactionEvent ]
		ifFalse: [ ZnServerTransactionEvent ].
	^ (self newLogEvent: logEventClass)
		request: request;
		response: response;
		timing: timing;
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logServerWriteError: exception [
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerWriteErrorEvent)
		exception: exception;
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logStarted [
	logLevel < 1 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerStartedEvent)
		description: self serverProcessName;
		emit
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> logStopped [
	logLevel < 1 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerStoppedEvent)
		description: self serverProcessName;
		emit
]

{ #category : #logging }
ZnSingleThreadedServer >> logToTranscript [
	"Setup logging to the Transcript.
	If logging was completely off, turn it on"

	logLevel isZero ifTrue: [ self loggingOn ].
	ZnLogEvent logToTranscript 
]

{ #category : #logging }
ZnSingleThreadedServer >> loggingOff [
	"Turn logging completely off - do not generate log events."
	
	self logLevel: 0
]

{ #category : #logging }
ZnSingleThreadedServer >> loggingOn [
	"Turn full logging on - generate all log events"
	
	self logLevel: 3
]

{ #category : #'private logging' }
ZnSingleThreadedServer >> newLogEvent: logEventClass [
	^ logEventClass new
		serverId: self route;
		yourself
]

{ #category : #private }
ZnSingleThreadedServer >> noteAcceptWaitTimedOut [
	"The accept wait on a server socket timed out (see #acceptWaitTimeout).
	This can be used as an opportunity to do some periodic tasks."
	
	self periodicTasks
]

{ #category : #accessing }
ZnSingleThreadedServer >> onRequestRespond: block [
	"Convenience method to quickly set up a delegate.
	On any incoming request, block will be executed with the request 
	as argument to produce a valid response."
	
	self delegate: (ZnValueDelegate with: block)  
]

{ #category : #private }
ZnSingleThreadedServer >> periodicTasks [
	"Every #acceptWaitTimeout seconds this method is called.
	Note that during this time the server is not actively listening for connections."
	
	sessionManager ifNotNil: [ sessionManager cleanupInvalidSessions ]
]

{ #category : #printing }
ZnSingleThreadedServer >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(.
	stream nextPutAll: (self isRunning ifTrue: [ 'running' ] ifFalse: [ 'stopped' ]).
	self port ifNotNil: [ :port | stream space; print: port ].
	self bindingAddress ifNotNil: [ :bindingAddress | stream space; print: bindingAddress ].
	stream nextPut: $)
]

{ #category : #accessing }
ZnSingleThreadedServer >> process [
	"Return the process that is running my main listening loop.
	Will be nil when I am not running"
	
	^ process
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> readRequest: stream timing: timing [
	"Read a request from stream.
	We add a virtual header containing the remote IP address of the client"

	| request initialMilliseconds |
	initialMilliseconds := Time millisecondClockValue.
	request := self withMaximumEntitySizeDo: [ 
		self withMaximumNumberOfDictionaryEntriesDo: [ 
			self withDefaultCharacterEncoderDo: [
				self reader value: stream ] ] ].
	request headers 
		at: ZnConstants remoteAddressHeader
		put: (ZnNetworkingUtils ipAddressToString: stream socket remoteAddress).
	timing requestDuration:  (self logRequestRead: request started: initialMilliseconds).
	^ request
]

{ #category : #private }
ZnSingleThreadedServer >> releaseServerSocket [
	"Release our server socket"
	
	| address port |
	serverSocket ifNil: [ ^ self ].
	address := [ serverSocket address ] on: Error do: [ nil ].
	port := [ serverSocket port ]on: Error do: [ nil ].
	(Delay forMilliseconds: 10) wait.
	serverSocket destroy.
	serverSocket := nil.
	self logServerSocketReleasedAddress: address port: port 
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> serveConnectionOn: listeningSocket [
	"We wait up to acceptWaitTimeout seconds for an incoming connection.
	If we get one we wrap it in a SocketStream and #executeOneRequestResponseOn: on it.
	We do not fork a worker thread/process but stay in the current one for just one request/response cycle."

	| stream socket |
	socket := listeningSocket waitForAcceptFor: self acceptWaitTimeout.
	socket ifNil: [ ^ self noteAcceptWaitTimedOut ].
	stream := self socketStreamOn: socket.
	[ 
	[ self executeOneRequestResponseOn: stream ]
		ensure: [ 
			self logConnectionClosed: stream.
			stream close ] ]
		ifCurtailed: [ 
			socket destroy ]
]

{ #category : #private }
ZnSingleThreadedServer >> serverProcessName [
	^ String streamContents: [ :stream |
		stream nextPutAll: self class name; nextPutAll: ' HTTP port '; print: self port ]
]

{ #category : #accessing }
ZnSingleThreadedServer >> serverSocket [
	"Return the server socket that I am using.
	Will be nil when I am not running"
	
	^ serverSocket
]

{ #category : #private }
ZnSingleThreadedServer >> socketStreamOn: socket [
	self logConnectionAccepted: socket.
	^ ZnNetworkingUtils socketStreamOn: socket
]

{ #category : #public }
ZnSingleThreadedServer >> start [
	"Start me. I will start listening on my port for incoming HTTP connections.
	If I am running, I will first stop and thus effectively restart"
	
	self stop: false.
	self class default = self ifTrue: [ self register ].
	process := [ [ self listenLoop ] repeat ]
		forkAt: Processor highIOPriority 
		named: self serverProcessName.
	self logStarted.

]

{ #category : #public }
ZnSingleThreadedServer >> stop: unregister [
	"Stop me. I will stop listening on my port for incoming HTTP connections.
	If unregister is true, unregister me from the list of managed instances.
	Does nothing when I am not running"
	
	self isRunning ifFalse: [ ^ self ].
	process terminate. 
	process := nil.
	unregister ifTrue: [ self unregister ].
	self closeDelegate.
	self logStopped
]

{ #category : #private }
ZnSingleThreadedServer >> withDefaultCharacterEncoderDo: block [
	^ self defaultEncoder = ZnDefaultCharacterEncoder value
			ifTrue: block
			ifFalse: [ 
				ZnDefaultCharacterEncoder 
					value: self defaultEncoder  
					during: block ]
]

{ #category : #private }
ZnSingleThreadedServer >> withMaximumEntitySizeDo: block [
	^ self maximumEntitySize = ZnMaximumEntitySize value
			ifTrue: block
			ifFalse: [ 
				ZnMaximumEntitySize 
					value: self maximumEntitySize  
					during: block ]
]

{ #category : #private }
ZnSingleThreadedServer >> withMaximumNumberOfDictionaryEntriesDo: block [
	^ self maximumNumberOfDictionaryEntries = ZnMaximumNumberOfDictionaryEntries value
			ifTrue: block
			ifFalse: [ 
				ZnMaximumNumberOfDictionaryEntries 
					value: self maximumNumberOfDictionaryEntries 
					during: block ]
]

{ #category : #'request handling' }
ZnSingleThreadedServer >> writeResponse: response on: stream timing: timing [
	"Write response to stream and flush the stream"
	
	| initialMilliseconds |
	initialMilliseconds := Time millisecondClockValue.
	self withDefaultCharacterEncoderDo: [ response writeOn: stream ].
	stream flush.
	timing responseDuration: (self logResponseWritten: response started: initialMilliseconds)  
]
