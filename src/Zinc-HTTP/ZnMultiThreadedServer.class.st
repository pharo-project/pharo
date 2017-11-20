"
I am ZnMultiThreadedServer.
I inherit most features from ZnSingleThreadedServer.

  ZnMultiThreadedServer startDefaultOn: 1701.
  ZnMultiThreadedServer default authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
  ZnClient new username: 'foo' password: 'secret'; get: 'http://localhost:1701'.

I am multi threaded, I fork a new process for each incoming request.
I try to keep connections alive in each process.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnMultiThreadedServer,
	#superclass : #ZnSingleThreadedServer,
	#category : #Zinc-HTTP-Client-Server
}

{ #category : #'request handling' }
ZnMultiThreadedServer >> augmentResponse: response forRequest: request [
	"Our handler has produced response for request, manipulate the response before writing it"

	super augmentResponse: response forRequest: request.
	response setKeepAliveFor: request.
	response setConnectionCloseFor: request
]

{ #category : #private }
ZnMultiThreadedServer >> closeSocketStream: socketStream [
	[ socketStream close ] 
		on: Exception 
		do: [ ]
]

{ #category : #private }
ZnMultiThreadedServer >> exceptionSet: classNames [
	^ classNames
		inject: ExceptionSet new
		into: [ :set :each |
			(Smalltalk includesKey: each)
				ifTrue: [ set add: (Smalltalk at: each); yourself ]
				ifFalse: [ set ] ]
]

{ #category : #'request handling' }
ZnMultiThreadedServer >> executeOneRequestResponseOn: stream [
	"Execute one HTTP request / response cycle on stream in 3 steps
	#readRequest: #handleRequest: and #writeResponse:on:
	Return true when the outer loop we are in should stop."

	| request response timing|
	timing := ZnServerTransactionTiming new.
	(request := self readRequestSafely: stream timing: timing)
		ifNil: [ ^ true ]
		ifNotNil: [ 
			response := self handleRequest: request timing: timing.
			self augmentResponse: response forRequest: request.
			self writeResponseSafely: response on: stream timing: timing.
			self logServerTransactionRequest: request response: response timing: timing.
			response useConnection: stream ].
	^ request wantsConnectionClose or: [ response wantsConnectionClose ]
]

{ #category : #'request handling' }
ZnMultiThreadedServer >> executeRequestResponseLoopOn: stream [
	"Execute the HTTP request / response loop on stream one cycle at a time
	until the other end indicates it wants to stop or times out"

	ZnCurrentServer 
		value: self 
		during: [ 
			[ self executeOneRequestResponseOn: stream ] whileFalse ]
]

{ #category : #'request handling' }
ZnMultiThreadedServer >> listenLoop [
	"We create a listening Socket, then wait for a connection.
	After each connection we also check that the listening Socket is still valid 
	- if not we just make a recursive call to this method to start over."

	self initializeServerSocket.
	[ [ 
		serverSocket isValid 
			ifFalse: [
				"will trigger #ifCurtailed: block and destroy socket"
				^ self listenLoop ].
		self serveConnectionsOn: serverSocket ] repeat ]

		ifCurtailed: [ self releaseServerSocket ]
]

{ #category : #private }
ZnMultiThreadedServer >> readRequestBadExceptionSet [
	"Return the set of exceptions which, when they occur while reading a request,
	are interpreted as equivalent to a request parse error or bad request."

	^ self
		exceptionSet:
			#(#ZnParseError #ZnCharacterEncodingError #ZnUnknownScheme #ZnPortNotANumber #ZnTooManyDictionaryEntries #ZnEntityTooLarge)
]

{ #category : #'request handling' }
ZnMultiThreadedServer >> readRequestSafely: stream timing: timing [
	"Read request from stream, returning nil when the connection is closed or times out"

	^ [ 
		[ self readRequest: stream timing: timing ]
			on: self readRequestBadExceptionSet
			do: [ :exception | 
				self logServerReadError: exception.
				self writeResponseBad: exception on: stream timing: timing.
				nil ] ]
			on: self readRequestTerminationExceptionSet
			do: [ :exception | 
				self logServerReadError: exception.
				nil ]
]

{ #category : #private }
ZnMultiThreadedServer >> readRequestTerminationExceptionSet [
	"Return the set of exceptions which, when they occur while reading a request,
	are interpreted as equivalent to a timeout or connection close."

	^ self
		exceptionSet:
			#(#ConnectionClosed #ConnectionTimedOut #PrimitiveFailed)
]

{ #category : #'request handling' }
ZnMultiThreadedServer >> serveConnectionsOn: listeningSocket [
	"We wait up to acceptWaitTimeout seconds for an incoming connection.
	If we get one we wrap it in a SocketStream and #executeRequestResponseLoopOn: on it"
		 
	| stream socket |
	socket := listeningSocket waitForAcceptFor: self acceptWaitTimeout.
	socket ifNil: [ ^ self noteAcceptWaitTimedOut ].
	stream := self socketStreamOn: socket.
	[ [ [ self executeRequestResponseLoopOn: stream ]
		ensure: [ self logConnectionClosed: stream. self closeSocketStream: stream ] ]
			ifCurtailed: [ socket destroy ] ]
				forkAt: Processor lowIOPriority
				named: self workerProcessName 
]

{ #category : #private }
ZnMultiThreadedServer >> workerProcessName [
	^ String streamContents: [ :stream |
		stream nextPutAll: self class name; nextPutAll: ' HTTP worker' ]
]

{ #category : #private }
ZnMultiThreadedServer >> writeResponseBad: exception on: stream timing: timing [
	"When we failed to parse a request, write a bad request response before closing."

	| response |
	response := ZnResponse new
		statusLine: ZnStatusLine badRequest;
		headers: ZnHeaders defaultResponseHeaders;
		entity: (ZnEntity textCRLF: 'Bad Request ' , exception printString);
		yourself.
	response setConnectionClose.
	self writeResponseSafely: response on: stream timing: timing
]

{ #category : #'request handling' }
ZnMultiThreadedServer >> writeResponseSafely: response on: stream timing: timing [
	"Write response to stream, when the connection is closed or times out we ignore this"

	[ self writeResponse: response on: stream timing: timing ]
		on: self writeResponseTerminationExceptionSet
		do: [ :exception | self logServerWriteError: exception ]
]

{ #category : #private }
ZnMultiThreadedServer >> writeResponseTerminationExceptionSet [
	"Return the set of exceptions which, when they occur while writing a response,
	are interpreted as equivalent to a timeout or connection close."

	^ self exceptionSet: #(#ConnectionClosed #ConnectionTimedOut #PrimitiveFailed)
]
