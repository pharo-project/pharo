"
I am ZnNetworkingUtils holding various utilities related to HTTP networking.

My class side is a general API.

My instance side is a factory.

ZnNetworkingUtils default socketStreamClass: SocketStream.
ZnNetworkingUtils default socketStreamClass: ZdcSocketStream.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnNetworkingUtils,
	#superclass : #Object,
	#instVars : [
		'socketStreamClass',
		'secureSocketStreamClass'
	],
	#classVars : [
		'Default',
		'DefaultSocketStreamTimeout'
	],
	#category : 'Zinc-HTTP-Support'
}

{ #category : #accessing }
ZnNetworkingUtils class >> default [
	^ Default ifNil: [ Default := self new ]
]

{ #category : #accessing }
ZnNetworkingUtils class >> default: instance [
	Default := instance 
]

{ #category : #constants }
ZnNetworkingUtils class >> defaultSocketStreamTimeout [
	"Global default timeout in seconds for SocketStream IO"
	
	^ DefaultSocketStreamTimeout
]

{ #category : #constants }
ZnNetworkingUtils class >> defaultSocketStreamTimeout: seconds [
	"Global default timeout in seconds for SocketStream IO"
	
	^ DefaultSocketStreamTimeout := seconds
]

{ #category : #'class initialization' }
ZnNetworkingUtils class >> initialize [
	"The default will try to use ZdcSocketStream and ZdcSecureSocketStream"
	 
	Default := self new.
	DefaultSocketStreamTimeout := 30
]

{ #category : #converting }
ZnNetworkingUtils class >> ipAddressToString: byteArray [
	^ String streamContents: [ :stream |
			byteArray 
				do: [ :each | stream print: each ] 
				separatedBy: [ stream nextPut: $. ] ]
]

{ #category : #constants }
ZnNetworkingUtils class >> listenBacklogSize [
	"Server socket backlog size (number of pending connection waiting to be accepted)"
	
	^ 32
]

{ #category : #proxy }
ZnNetworkingUtils class >> proxyAuthorizationHeaderValueToUrl: url [
	"Answer the Proxy-Authorization header value for HTTP access to url, or nil if none"
	
	^ self default proxyAuthorizationHeaderValueToUrl: url  
]

{ #category : #proxy }
ZnNetworkingUtils class >> proxyUrl [
	"Answer the host:port URL the HTTP/S proxy"
	
	^ self default proxyUrl
]

{ #category : #networking }
ZnNetworkingUtils class >> secureSocketStreamOn: socket [
	^ self default 
		secureSocketStreamOn: socket  
]

{ #category : #networking }
ZnNetworkingUtils class >> serverSocketOn: port [
	^ self default 
		serverSocketOn: port  
]

{ #category : #networking }
ZnNetworkingUtils class >> serverSocketOn: port interface: address [
	^ self default 
		serverSocketOn: port interface: address 
]

{ #category : #proxy }
ZnNetworkingUtils class >> shouldProxyUrl: url [
	"Answer if we should proxy HTTP access to url."
	
	^ self default shouldProxyUrl: url  
]

{ #category : #constants }
ZnNetworkingUtils class >> socketBufferSize [
	"Size in bytes for Sockets and SocketStream IO buffers"

	^ 4096
]

{ #category : #networking }
ZnNetworkingUtils class >> socketStreamOn: socket [
	^ self default 
		socketStreamOn: socket  
]

{ #category : #constants }
ZnNetworkingUtils class >> socketStreamTimeout [
	"Access the current timeout in seconds for SocketStream IO"
	
	^ ZnConnectionTimeout value 
		ifNil: [ self defaultSocketStreamTimeout ]
]

{ #category : #networking }
ZnNetworkingUtils class >> socketStreamToUrl: url [
	^ self default
		socketStreamToUrl: url  
]

{ #category : #accessing }
ZnNetworkingUtils >> bufferSize [
	^ self class socketBufferSize
]

{ #category : #proxy }
ZnNetworkingUtils >> httpProxyPassword [
	"Return the password for proxy authorization"
	
	^ NetworkSystemSettings proxyPassword 
]

{ #category : #proxy }
ZnNetworkingUtils >> httpProxyPort [
	"Return the port of the proxy server to use"
	
	^ NetworkSystemSettings httpProxyPort
]

{ #category : #proxy }
ZnNetworkingUtils >> httpProxyServer [
	"Return the hostname of the proxy server to use"
	
	^ NetworkSystemSettings httpProxyServer
]

{ #category : #proxy }
ZnNetworkingUtils >> httpProxyUser [
	"Return the user for proxy authorization"
	
	^ NetworkSystemSettings proxyUser
]

{ #category : #initialization }
ZnNetworkingUtils >> initialize [
	super initialize.
	self socketStreamClass: (Smalltalk globals at: #ZdcSocketStream ifAbsent: [ SocketStream ]).
	self secureSocketStreamClass: (Smalltalk globals at: #ZdcSecureSocketStream ifAbsent: [ nil ])
]

{ #category : #proxy }
ZnNetworkingUtils >> isProxyAuthorizationRequired [
	"Does the proxy require authorization ?"
	
	^ self isProxySet and: [ NetworkSystemSettings useNetworkAuthentification ]
]

{ #category : #proxy }
ZnNetworkingUtils >> isProxySet [
	"Should a proxy be used ?"
	
	^ NetworkSystemSettings useHTTPProxy 
]

{ #category : #proxy }
ZnNetworkingUtils >> proxyAuthorizationHeaderValueToUrl: url [
	"Answer the Proxy-Authorization header value for HTTP access to url, or nil if none"
	
	^ (self isProxyAuthorizationRequired and: [ self shouldProxyUrl: url ])
		ifTrue: [ 'Basic ', (ZnUtils encodeBase64: (self httpProxyUser, ':', self httpProxyPassword)) ] 
		ifFalse: [ nil ]  
]

{ #category : #proxy }
ZnNetworkingUtils >> proxyUrl [
	"System settings do currently not support HTTPS proxies."

	^ ZnUrl new
		scheme: #http;
		host: self httpProxyServer;
		port: self httpProxyPort;
		yourself
]

{ #category : #accessing }
ZnNetworkingUtils >> secureSocketStreamClass [
	^ secureSocketStreamClass ifNil: [ 
		secureSocketStreamClass := Smalltalk globals at: #ZdcSecureSocketStream ifAbsent: [ nil ] ]
]

{ #category : #initialize-release }
ZnNetworkingUtils >> secureSocketStreamClass: anObject [
	secureSocketStreamClass := anObject
]

{ #category : #public }
ZnNetworkingUtils >> secureSocketStreamOn: socket [
	| stream |
	stream := self secureSocketStreamClass on: socket.
	self setSocketStreamParameters: stream.  
	^ stream
]

{ #category : #public }
ZnNetworkingUtils >> serverSocketOn: port [
	| socket |
	socket := Socket newTCP.
	self setServerSocketOptions: socket.
	socket listenOn: port backlogSize: self class listenBacklogSize.
	socket isValid
		ifFalse: [ self error: 'Cannot create socket on port ', port printString ].
	^ socket
]

{ #category : #public }
ZnNetworkingUtils >> serverSocketOn: port interface: address [
	| socket |
	socket := Socket newTCP.
	self setServerSocketOptions: socket.
	socket 
		listenOn: port 
		backlogSize: self class listenBacklogSize 
		interface: address.
	socket isValid
		ifFalse: [ self error: 'Cannot create socket on port ', port printString ].
	^ socket
]

{ #category : #private }
ZnNetworkingUtils >> setServerSocketOptions: socket [
	socket
		setOption: 'TCP_NODELAY' value: 1;
		setOption: 'SO_SNDBUF' value: self class socketBufferSize;
		setOption: 'SO_RCVBUF' value: self class socketBufferSize 
]

{ #category : #private }
ZnNetworkingUtils >> setSocketStreamParameters: stream [
	stream 
		binary;
		shouldSignal: true;
		autoFlush: false;
		bufferSize: self bufferSize;
		timeout: self timeout.

]

{ #category : #proxy }
ZnNetworkingUtils >> shouldProxyUrl: url [
	"Even when a proxy is set, some URLs should not be proxied.
	There is no setting for this in the image.
	We exclude localhost and explicit exceptions from being proxied."
	
	self isProxySet ifFalse: [ ^ false ].
	^ url isLocalHost not
		and: [ 
			[ (NetworkSystemSettings isAnExceptionFor: url) not ]
				on: MessageNotUnderstood
				do: [ true ] ]
]

{ #category : #accessing }
ZnNetworkingUtils >> socketStreamClass [
	^ socketStreamClass ifNil: [ socketStreamClass := SocketStream ]
]

{ #category : #initialize-release }
ZnNetworkingUtils >> socketStreamClass: anObject [
	socketStreamClass := anObject
]

{ #category : #public }
ZnNetworkingUtils >> socketStreamOn: socket [
	| stream |
	stream := self socketStreamClass on: socket.
	self setSocketStreamParameters: stream.  
	^ stream
]

{ #category : #proxy }
ZnNetworkingUtils >> socketStreamToProxy [
	^ self socketStreamToUrlDirectly: self proxyUrl
]

{ #category : #public }
ZnNetworkingUtils >> socketStreamToUrl: url [
	url hasHost ifFalse: [ ZnMissingHost signal ].  
	^ (self shouldProxyUrl: url)
		ifTrue: [ self socketStreamToProxy ] 
		ifFalse: [ self socketStreamToUrlDirectly: url ]
]

{ #category : #public }
ZnNetworkingUtils >> socketStreamToUrlDirectly: url [
	| stream address |
	address := NetNameResolver addressForName: url host timeout: self timeout. 
	stream := (self streamClassForScheme: url scheme) 
		openConnectionToHost: address
		port: url portOrDefault
		timeout: self timeout.  
	self setSocketStreamParameters: stream. 
	"note that for TLS/SSL, #connect should be called"
	^ stream
]

{ #category : #accessing }
ZnNetworkingUtils >> streamClassForScheme: scheme [
	(#(http ws) includes: scheme) ifTrue: [ 
		^ self socketStreamClass ].
	(#(https wss) includes: scheme) ifTrue: [ 
		^ self secureSocketStreamClass ifNil: [
			self error: 'No secure socket stream class set or available' ] ].
	(ZnUnknownScheme scheme: scheme) signal
]

{ #category : #accessing }
ZnNetworkingUtils >> timeout [
	^ self class socketStreamTimeout
]
