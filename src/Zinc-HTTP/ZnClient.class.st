"
I am ZnClient, an object to build, execute and process HTTP client requests.

I have a rich protocol to construct requests and to access responses.
I have various error handling options.
I can reuse an existing connection to a specific host:port.
I handle sessions, cookies, redirects and authentication.
I have many options (settings) with sensible defaults.

Simplest possible invocation:

	ZnClient new
		get: 'http://zn.stfx.eu/zn/numbers.txt'.
	
Using some features to make a better HTTP request:

	ZnClient new
		systemPolicy;
		accept: ZnMimeType textPlain;
		http;
		host: 'zn.stfx.eu';
		path: 'zn/numbers.txt';
		contentReader: [ :entity | entity contents lines collect: [ :each | each asNumber ] ];
		ifFail: [ :exception | self inform: 'I am sorry: ', exception printString ];
		get.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnClient,
	#superclass : #Object,
	#instVars : [
		'request',
		'response',
		'connection',
		'lastUsed',
		'options',
		'session',
		'logLevel'
	],
	#category : #'Zinc-HTTP-Client-Server'
}

{ #category : #'accessing request' }
ZnClient >> accept: mimeTypeObject [
	"Set the Accept HTTP header to mimeTypeObject for the current request."

	| mimeType |
	mimeType := mimeTypeObject asZnMimeType.
	self request setAccept: mimeType
]

{ #category : #'accessing request' }
ZnClient >> addPart: mimePart [
	"Add mimePart to the multipart/form-data entity of the current request"

	self multiPartFormDataEntity 
		addPart: mimePart 
]

{ #category : #'accessing request' }
ZnClient >> addPath: object [
	"Extend the path of the current request with object.
	Either a string or a collection of strings is allowed as argument."
	
	object isString
		ifTrue: [ self addPathSegment: object ] 
		ifFalse: [ object do: [ :each | self addPathSegment: each ] ]
]

{ #category : #'accessing request' }
ZnClient >> addPathSegment: pathSegment [
	"Extend the path of the current request with a single pathSegment."
	
	request url 
		ifNil: [ request url: (ZnUrl new addPathSegment: pathSegment; yourself) ]
		ifNotNil: [ request url addPathSegment: pathSegment ]  
]

{ #category : #'accessing request' }
ZnClient >> applicationFormUrlEncodedEntity [
	"Initialize the current request to have an application/x-www-form-urlencoded entity.
	Return the existing entity if there is one. 
	Fail if another type was already set."
	
	request hasEntity
		ifTrue: [
			request entity contentType = ZnMimeType applicationFormUrlEncoded 
				ifFalse: [ self error: 'An incompatible entity was already set for the request' ] ]
		ifFalse: [
			request entity: ZnApplicationFormUrlEncodedEntity new ].
	^ request entity
]

{ #category : #options }
ZnClient >> autoResetEntityMethods [
	"Return a collection of HTTP methods for which to auto #resetEntity.
	The default is to do this only for HEAD and DELETE, although most people 
	would want it for GET too, but this can be used for forms"
	
	^ self optionAt: #autoResetEntityMethods ifAbsentPut: [ #(HEAD DELETE) ]
]

{ #category : #options }
ZnClient >> autoResetEntityMethods: methods [
	"Set the collection of HTTP methods for which to auto #resetEntity.
	The default is to do this only for HEAD and DELETE, although most people 
	would want it for GET too, but this can be used for forms"
	
	^ self optionAt: #autoResetEntityMethods put: methods
]

{ #category : #options }
ZnClient >> beBinary [
	"Configure me to read HTTP responses without interpreting them further,
	their resources/entities will remain bytes."
	
	self reader: [ :stream | ZnResponse readBinaryFrom: stream ]
]

{ #category : #options }
ZnClient >> beOneShot [
	"Switch me to use connections only for a single request/response cycle.
	By default, I will keep HTTP connections open according to the HTTP 1.1 spec.
	When oneShot is true, I will add a Connection: close header to requests and close after one cycle.
	I cannot be one shot when #streaming is true because the stream would be closed too soon."
	
	self streaming
		ifTrue: [ self error: 'I cannot be one shot when #streaming is true.' ].
	self optionAt: #oneShot put: true   
]

{ #category : #private }
ZnClient >> canReuseConnection [
	self isConnected ifFalse: [ ^ false ].
	lastUsed ifNil: [ ^ false ].
	^ Time totalSeconds - lastUsed < self connectionReuseTimeout
]

{ #category : #options }
ZnClient >> certificate [
	"Return the TLS/SSL certificate to be used, or nil for none."
	
	^ self optionAt: #certificate ifAbsent: [ nil ]
]

{ #category : #options }
ZnClient >> certificate: anObject [
	"Set the TLS/SSL certificate to be used to anObject, or nil for none.
	See ZdcAbstractSSLSession>>#certificateName: for more information."
	
	^ self optionAt: #certificate put: anObject
]

{ #category : #private }
ZnClient >> cleanupConnection [
	(self isOneShot or: [ request notNil and: [ request wantsConnectionClose ] ])
		ifTrue: [
			self close ]
]

{ #category : #options }
ZnClient >> clientId [
	"Return the id for this HTTP client. Defaults to nil.
	Can be used identify different clients' log events."
	
	^ self optionAt: #clientId ifAbsent: [ nil ]
]

{ #category : #options }
ZnClient >> clientId: anObject [
	"Set the id for this HTTP client to anObject. Defaults to nil.
	Can be used identify different clients' log events."
	
	^ self optionAt: #clientId put: anObject
]

{ #category : #cleanup }
ZnClient >> close [
	"Close any HTTP network connection that I might have kept open."

	connection
		ifNotNil: [ 
			| address port |
			address := [ connection socket remoteAddress ] on: Error do: [ nil ].
			port := [ connection socket remotePort ] on: Error do: [ nil ].
			connection close.
			connection := nil.
			self logConnectionClosedAddress: address port: port ].
	lastUsed := nil
]

{ #category : #accessing }
ZnClient >> connection [
	"Return the socket stream connection that I am currently using"
	
	^ connection
]

{ #category : #options }
ZnClient >> connectionReuseTimeout [
	"Return the timeout in seconds currently used to determine
	if I should try to reuse an open connection. Defaults to 10 seconds."

	^ self optionAt: #connectionReuseTimeout ifAbsent: [ 10 ]
]

{ #category : #options }
ZnClient >> connectionReuseTimeout: seconds [
	"Set the timeout used to determine if I should try 
	to reuse an open connection to seconds."

	self optionAt: #connectionReuseTimeout put: seconds
]

{ #category : #options }
ZnClient >> contentReader [
	"Return the current content reader, an object conforming 
	to the #value: protocol to convert a response entity
	as used by #contents. Defaults to nil.
	Can be used to parse incoming data."
	
	^ self optionAt: #contentReader ifAbsent: [ nil ]
]

{ #category : #options }
ZnClient >> contentReader: object [
	"Set the content reader, an object conforming 
	to the #value: protocol to convert a response entity
	as used by #contents to object.
	Can be used to parse incoming data."

	self optionAt: #contentReader put: object
]

{ #category : #'accessing request' }
ZnClient >> contentType: mimeType [
	"Set the content-type of the request entity, provided we have one"
	
	request hasEntity
		ifTrue: [ 
			request entity contentType: mimeType asZnMimeType ]
		ifFalse: [ 
			self error: 'There is no request entity yet, cannot set its content-type' ]
]

{ #category : #options }
ZnClient >> contentWriter [
	"Return the current content writer, an object conforming 
	to the #value: protocol to convert an object to a request content entity
	as used by #contents:. Defaults to nil.
	Can be used to format data to be uploaded."

	^ self optionAt: #contentWriter ifAbsent: [ nil ]
]

{ #category : #options }
ZnClient >> contentWriter: object [
	"Set current content writer, an object conforming 
	to the #value: protocol to convert an object to a request content entity
	as used by #contents:. Defaults to nil.
	Can be used to format data to be uploaded."

	self optionAt: #contentWriter put: object
]

{ #category : #'accessing response' }
ZnClient >> contents [ 
	"Answer the contents or HTTP payload of the last response, 
	assuming the current request was executed.
	If I have a contentReader, it will be used to compute the contents.
	Otherwise I delegate to ZnResponse>>#contents, unless I am #streaming,
	in which case I return the stream of the streaming entity."

	^ response ifNotNil: [ 
		self contentReader 
			ifNil: [ 
				self streaming
					ifTrue: [  response entity stream ]
					ifFalse: [ response contents ] ]
			ifNotNil: [ :reader | reader value: response entity ] ]  
]

{ #category : #'accessing request' }
ZnClient >> contents: object [
	"Set the entity contents of the current request to object.
	I will pass this through my contentWriter if I have one.
	Otherwise I delegate to ZnEntity to convert object to a generic entity."
	
	self contentWriter 
		ifNil: [ self entity: (ZnEntity with: object) ]
		ifNotNil: [ :writer | self entity: (writer value: object) ]
]

{ #category : #convenience }
ZnClient >> curl [
	"Create and return a curl command line invocation equivalent to the current request."
	"ZnClient new url: 'http://zn.stfx.eu/zn/small.html'; method: #GET; curl."
	"ZnClient new url: 'http://zn.stfx.eu/echo'; contents: #[2 4 6 8]; method: #POST; curl."
	
	self prepareRequest.
	^ String streamContents: [ :stream |
		request hasEntity ifTrue: [ 
			 stream << 'echo ' << request entity asByteArray hex << ' | xxd -r -p | ' ].
		stream << 'curl'.
		request method = #GET ifFalse: [ stream << ' -X ' << request method ].
		stream space; print: request uri.
		request headersDo: [ :key :value |
			stream << ' -H"' << key << ':' << value << '"' ].
		request hasEntity ifTrue: [ stream << ' --data-binary @-' ] ]
]

{ #category : #initialization }
ZnClient >> debugPolicy [
	self 
		beOneShot
]

{ #category : #options }
ZnClient >> defaultEncoder [
	"The default character encoder to use when none is set in a mime-type"
	
	^ self optionAt: #defaultEncoder ifAbsent: [ ZnDefaultCharacterEncoder value ] 
]

{ #category : #options }
ZnClient >> defaultEncoder: encoder [
	"Set the default character encoder to use when none is set in a mime-type"
	
	^ self optionAt: #defaultEncoder put: encoder asZnCharacterEncoder
]

{ #category : #operations }
ZnClient >> delete [
	"Execute a HTTP DELETE ont the request set up and return the response #contents."
	
	^ self 
		method: #DELETE; 
		execute
]

{ #category : #convenience }
ZnClient >> delete: url [
	"Execute an HTTP DELETE request on url and return the response #contents."
	
	^ self 
		url: url; 
		delete
]

{ #category : #options }
ZnClient >> dontFollowRedirects [
	"Disable the default automatic following of HTTP redirects.
	Redirects will be returned as normal responses."

	^ self followRedirects: false
]

{ #category : #convenience }
ZnClient >> downloadEntityTo: path [
	"If I hold an entity, download it directly to a file.
	If path describes a file, use that. 
	If path describes a directory, use the last path segment of the URI to construct a file in that directory.
	The URI of the last request is used, which could make a difference when following redirects."

	| target |
	self entity ifNil: [ ^ self ].
	target := ZnFileSystemUtils downloadPathTo: path for: self request uri lastPathSegment.
	ZnFileSystemUtils
		newBinaryFileNamed: target
		do: [ :fileStream |
			self withProgressDo: [ self entity writeOn: fileStream ] ]
]

{ #category : #convenience }
ZnClient >> downloadTo: path [
	"Setup streaming, do a GET request and download the entity directly to a file.
	If path describes a file, use that. 
	If path describes a directory, use the last path segment of the URI to construct a file in that directory.
	The URI of the first request is used, which could make a difference when following redirects.
	Return true when successful, false otherwise."

	| target |
	target := ZnFileSystemUtils downloadPathTo: path for: self request uri lastPathSegment.
	self
		streaming: true;
		get.
	^ self isSuccess
		ifTrue: [ 
			self downloadEntityTo: target.
			true ]
		ifFalse: [ false ]
]

{ #category : #options }
ZnClient >> enforceAcceptContentType [
	"Return whether or not I enforce that the content type of the
	response matches the content type specified in the accept request header."
	
	^ self optionAt: #enforceAcceptContentType ifAbsent: [ false ]
]

{ #category : #options }
ZnClient >> enforceAcceptContentType: boolean [
	"Set whether or not I enforce that the content type of the
	response matches the content type specified in the accept request
	header to boolean."

	self optionAt: #enforceAcceptContentType put: boolean
]

{ #category : #options }
ZnClient >> enforceHttpSuccess [
	"Return whether or not I enforce that the response 
	to the request is an HTTP success code."
	
	^ self optionAt: #enforceHttpSuccess ifAbsent: [ false ]
]

{ #category : #options }
ZnClient >> enforceHttpSuccess: boolean [
	"Set whether or not I enforce that the response 
	to the request is an HTTP success code to boolean."

	self optionAt: #enforceHttpSuccess put: boolean
]

{ #category : #'accessing response' }
ZnClient >> entity [
	"Answer the entity or HTTP payload of the last response, 
	assuming the current request was executed."
	
	^ response ifNotNil: [ response entity ]  
]

{ #category : #'accessing request' }
ZnClient >> entity: entity [
	"Set the entity of the current request to enity.
	A ZnEntity subclass or compatible object is expected."
	
	request entity: entity
]

{ #category : #operations }
ZnClient >> execute [
	"Execute the currently set up request to generate a response.
	Return the #contents of the response, if any."

	| result |
	self prepareRequest.
	self withProgressDo: [ result := self executeWithTimeout ].
	self prepareNextRequest.
	^ result
]

{ #category : #'private protocol' }
ZnClient >> executeRequestResponse [
	| initialMilliseconds requestDuration responseDuration |
	initialMilliseconds := Time millisecondClockValue.
	self writeRequest.
	requestDuration := self logRequestWrittenStarted: initialMilliseconds.
	initialMilliseconds := Time millisecondClockValue.
	self readResponse.
	responseDuration := self logResponseReadStarted: initialMilliseconds.
	self logClientTransactionRequestDuration: requestDuration responseDuration: responseDuration
]

{ #category : #'private protocol' }
ZnClient >> executeWithRedirectsRemaining: redirectCount [
	self getConnectionAndExecute.
	response isRedirect 
		ifTrue: [
			(redirectCount > 0 and: [ self followRedirects ])
				ifTrue: [
					self 
						prepareRedirect; 
						executeWithRedirectsRemaining: redirectCount - 1 ]
				ifFalse: [
					self followRedirects 
						ifTrue: [ ZnTooManyRedirects signal ] ] ].
	^ self handleResponse
	
]

{ #category : #'private protocol' }
ZnClient >> executeWithRetriesRemaining: retryCount [
	^ [ self executeWithRedirectsRemaining: self maxNumberOfRedirects ]
		on: self retryExceptionSet
		do: [ :exception |
			retryCount > 0
				ifTrue: [
					self 
						handleRetry: exception;
						executeWithRetriesRemaining: retryCount - 1 ]
				ifFalse: [
					exception pass ] ]
]

{ #category : #'private protocol' }
ZnClient >> executeWithTimeout [
	^ self withTimeoutDo: [ 
		[ self executeWithRetriesRemaining: self numberOfRetries ]
			on: Error
			do: self ifFailBlock ]
]

{ #category : #options }
ZnClient >> followRedirects [
	"Return whether I will automatically following of HTTP redirects.
	This is still subject to #maxNumberOfRedirects. Defaults to true.
	When set to false, redirects will be returned as normal responses."

	^ self optionAt: #followRedirects ifAbsent: [ true ]
]

{ #category : #options }
ZnClient >> followRedirects: boolean [
	"Set whether or not I will automatically following of HTTP redirects.
	This is still subject to #maxNumberOfRedirects. Defaults to true.
	When set to false, redirects will be returned as normal responses."

	self optionAt: #followRedirects put: boolean 
]

{ #category : #'accessing request' }
ZnClient >> formAdd: association [
	"Add the key->value association to the application/x-www-form-urlencoded 
	entity of the current request."
	
	self applicationFormUrlEncodedEntity 
		add: association 
]

{ #category : #'accessing request' }
ZnClient >> formAddAll: keyedCollection [
	"Add all key->value associations of keyedCollection to 
	the application/x-www-form-urlencoded entity of the current request."

	self applicationFormUrlEncodedEntity 
		addAll: keyedCollection 
]

{ #category : #'accessing request' }
ZnClient >> formAt: key add: value [
	"Add key equals value to the application/x-www-form-urlencoded 
	entity of the current request. This is for multi-values form fields."

	self applicationFormUrlEncodedEntity 
		at: key add: value
]

{ #category : #'accessing request' }
ZnClient >> formAt: key put: value [
	"Set key equal to value in the application/x-www-form-urlencoded 
	entity of the current request."

	self applicationFormUrlEncodedEntity 
		at: key put: value
]

{ #category : #operations }
ZnClient >> get [
	"Execute an HTTP GET the request set up and return the response #contents."
	
	^ self 
		method: #GET; 
		execute
]

{ #category : #convenience }
ZnClient >> get: url [
	"Execute an HTTP GET request on url and return the response #contents."

	^ self 
		url: url; 
		get
]

{ #category : #'private protocol' }
ZnClient >> getConnectionAndExecute [
	self canReuseConnection 
		ifTrue: [ 
			[ ^ self executeRequestResponse; cleanupConnection ] 
				on: Exception 
				do: [ :exception | self logIgnoringExceptionOnReusedConnection: exception; close ] ].
	self newConnectionTo: request url.
	[ self executeRequestResponse ]
		ensure: [ self cleanupConnection ]
]

{ #category : #'private protocol' }
ZnClient >> handleResponse [
	self receiveCookies.
	(self enforceHttpSuccess and: [ response isSuccess not ])
		ifTrue: [ 
			(self followRedirects not and: [ response isRedirect ])
				ifFalse: [ (ZnHttpUnsuccessful response: response) signal ] ].
	(self enforceAcceptContentType and: [ self isContentTypeAcceptable not ])
		ifTrue: [ 
			(ZnUnexpectedContentType 
				expected: request accept asZnMimeType 
				actual: response contentType) signal ].
	^ self contents
]

{ #category : #'private protocol' }
ZnClient >> handleRetry: exception [
	self logRetrying: exception.
	self close.
	(Delay forSeconds: self retryDelay) wait.

]

{ #category : #operations }
ZnClient >> head [
	"Execute a HTTP HEAD on the request set up and return nil."

	^ self 
		method: #HEAD; 
		execute
]

{ #category : #convenience }
ZnClient >> head: url [
	"Execute an HTTP HEAD request on url and return nil."

	^ self 
		url: url; 
		head
]

{ #category : #'accessing request' }
ZnClient >> headerAddAll: keyedCollection [
	"Add all key/value pairs of keyedCollection as headers.
	Note that #addAllMulti: is used"
	
	request headers 
		addAll: keyedCollection 
]

{ #category : #'accessing request' }
ZnClient >> headerAt: key add: value [
	"Add value to key in the HTTP header of the current request.
	This is for multi-values headers."
	
	request headers 
		at: key add: value
]

{ #category : #'accessing request' }
ZnClient >> headerAt: key put: value [
	"Set key equals value in the HTTP header of the current request"
	
	request headers 
		at: key put: value   
]

{ #category : #'accessing request' }
ZnClient >> host: hostName [
	"Set the host of the current request to hostName."
	
	self url: (ZnUrl new host: hostName; yourself)
]

{ #category : #'accessing request' }
ZnClient >> http [
	"Set the scheme of the current request to HTTP."
	
	self url: (ZnUrl new scheme: #http; yourself)
]

{ #category : #'accessing request' }
ZnClient >> https [
	"Set the scheme of the current request to HTTPS."
	
	self url: (ZnUrl new scheme: #https; yourself)
]

{ #category : #options }
ZnClient >> ifFail: block [
	"Set the block that will be executed if the request fails.
	The exception of the failure will be passed as argument."
	
	self optionAt: #ifFailBlock put: block
]

{ #category : #options }
ZnClient >> ifFailBlock [
	"Return the block that I should execute if the request fails.
	Defaults to #pass-ing the exception up. 
	Accepts an exception as argument."
	
	^ self optionAt: #ifFailBlock ifAbsentPut: [ [ :exception | exception pass ] ]
]

{ #category : #initialization }
ZnClient >> initialize [
	request := ZnRequest empty.
	self loggingOn 
]

{ #category : #initialization }
ZnClient >> interactivePolicy [
	self 
		beOneShot;
		ifFail: [ :exception | 
			self inform: 'HTTP call failed with ', exception printString.
			nil ]
]

{ #category : #testing }
ZnClient >> isBadRequest [
	"Return true if the last executed HTTP request returned an HTTP bad request response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isBadRequest ]
]

{ #category : #testing }
ZnClient >> isConnected [
	"Return true if I hold an open HTTP connection."

	^ connection notNil and: [ connection isConnected ]
]

{ #category : #testing }
ZnClient >> isContentTypeAcceptable [
	"Return true if the contentType of the response of the last executed 
	HTTP request matches the accept header of the request."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ 
			response hasEntity not 
				or: [ response contentType matches: request accept asZnMimeType ] ]
]

{ #category : #testing }
ZnClient >> isCreated [
	"Return true it the last executed HTTP request returned an HTTP created response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isCreated ]
]

{ #category : #testing }
ZnClient >> isNotFound [
	"Return true it the last executed HTTP request returned an HTTP not found response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isNotFound ]
]

{ #category : #testing }
ZnClient >> isNotModified [
	"Return true it the last executed HTTP request returned an HTTP not modified response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isNotModified ]
]

{ #category : #options }
ZnClient >> isOneShot [
	"Return if I use connections only for a single request/response cycle.
	By default, I will keep HTTP connections open according to the HTTP 1.1 spec.
	When oneShot is true, I will add a Connection: close header to requests."

	^ self optionAt: #oneShot ifAbsent: [ false ]
]

{ #category : #testing }
ZnClient >> isRedirect [
	"Return true it the last executed HTTP request returned an HTTP redirect response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isRedirect ]
]

{ #category : #testing }
ZnClient >> isSuccess [ 
	"Return true it the last executed HTTP request returned an HTTP success response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isSuccess ]
]

{ #category : #'private logging' }
ZnClient >> logClientTransactionRequestDuration: requestDuration responseDuration: responseDuration [
	| logEventClass |
	logLevel < 1 ifTrue: [ ^ nil ].
	logEventClass := logLevel = 1
		ifTrue: [ ZnSimplifiedClientTransactionEvent ]
		ifFalse: [ ZnClientTransactionEvent ].
	^ (self newLogEvent: logEventClass)
		request: request;
		requestDuration: requestDuration;
		response: response; 
		responseDuration: responseDuration;
		emit;
		yourself
]

{ #category : #'private logging' }
ZnClient >> logConnectionClosedAddress: address port: port [
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnClientConnectionClosedEvent)
		address: address;
		port: port;
		emit
]

{ #category : #'private logging' }
ZnClient >> logConnectionEstablishedTo: url started: initialMilliseconds [
	| event |
	logLevel < 3 ifTrue: [ ^ nil ].
	(event := self newLogEvent: ZnConnectionEstablishedEvent)
		hostname: url host;
		port: url portOrDefault;
		address: ([ connection socket remoteAddress ] on: Error do: [ nil ]);
		duration: (Time millisecondsSince: initialMilliseconds).
	(ZnNetworkingUtils shouldProxyUrl: url)
		ifTrue: [ event proxy: ZnNetworkingUtils proxyUrl ].
	event emit
]

{ #category : #'private logging' }
ZnClient >> logIgnoringExceptionOnReusedConnection: exception [
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnClientIgnoringExceptionOnConnectionReuseEvent)
		exception: exception;
		emit
]

{ #category : #logging }
ZnClient >> logLevel: integer [
	"Set the log level to integer.
	0 - no logging
	1 - simplified transaction logging
	2 - detailed transaction logging
	3 - log everything"
	
	logLevel := integer
]

{ #category : #'private logging' }
ZnClient >> logRedirect: url [
	^ (self newLogEvent: ZnClientFollowingRedirectEvent)
		target: url;
		emit
]

{ #category : #'private logging' }
ZnClient >> logRequestWrittenStarted: initialMilliseconds [
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnRequestWrittenEvent)
				request: request;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]
]

{ #category : #'private logging' }
ZnClient >> logResponseReadStarted: initialMilliseconds [
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnResponseReadEvent)
				response: response;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]
]

{ #category : #'private logging' }
ZnClient >> logRetrying: exception [
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnClientRetryingEvent)
		exception: exception;
		emit
]

{ #category : #logging }
ZnClient >> logToTranscript [
	"Setup logging to the Transcript.
	If logging was completely off, turn it on"

	logLevel isZero ifTrue: [ self loggingOn ].
	ZnLogEvent logToTranscript 
]

{ #category : #logging }
ZnClient >> loggingOff [
	"Turn logging completely off - do not generate log events."
	
	self logLevel: 0
]

{ #category : #logging }
ZnClient >> loggingOn [
	"Turn full logging on - generate all log events"
	
	self logLevel: 3
]

{ #category : #options }
ZnClient >> maxNumberOfRedirects [
	"Return the maximum number of HTTP redirect that will be followed.
	Note that when the #followsRedirect option is false, 
	no redirects will ever be followed regardless of the value of this option.
	Defaults to 3. A ZnTooManyRedirects will be signalled when the count reaches zero."

	^ self optionAt: #maxNumberOfRedirects ifAbsent: [ 3 ]
]

{ #category : #options }
ZnClient >> maxNumberOfRedirects: count [
	"Set the maximum number of HTTP redirect that will be followed to count.
	Note that when the #followsRedirect option is false, 
	no redirects will ever be followed regardless of the value of this option.
	A ZnTooManyRedirects will be signalled when the count reaches zero."

	self optionAt: #maxNumberOfRedirects put: count
]

{ #category : #options }
ZnClient >> maximumEntitySize [
	"Return the maximum entity size in bytes that I will read from a stream before signalling ZnEntityTooLarge"
	
	^ self optionAt: #maximumEntitySize ifAbsent: [ ZnMaximumEntitySize value ]
]

{ #category : #options }
ZnClient >> maximumEntitySize: integer [
	"Set the maximum entity size in bytes that I will read from a stream before signalling ZnEntityTooLarge"
	
	^ self optionAt: #maximumEntitySize put: integer
]

{ #category : #options }
ZnClient >> maximumNumberOfDictionaryEntries [
	"Return the maximum number of entries allowed in ZnMutliValueDictionaries before signalling ZnTooManyDictionaryEntries. This protects us from overflow attacks."
		
	^ self
		optionAt: #maximumNumberOfDictionaryEntries
		ifAbsent: [ ZnMaximumNumberOfDictionaryEntries value ]
]

{ #category : #options }
ZnClient >> maximumNumberOfDictionaryEntries: anInteger [
	"Set the maximum number of entries allowed in ZnMutliValueDictionaries before signalling ZnTooManyDictionaryEntries. This protects us from overflow attacks."
	
	^ self
		optionAt: #maximumNumberOfDictionaryEntries
		put: anInteger
]

{ #category : #'accessing request' }
ZnClient >> method: httpMethod [
	"Set the HTTP method of the current request to httpMethod.
	An uppercase symbol such as #GET is expected.
	Note that for all methods in #autoResetEntityMethods #resetEntity is called automatically,
	since you don't want the entity of a previous request to be sent again"

	(self autoResetEntityMethods includes: httpMethod)
		ifTrue: [ self resetEntity ].
	request method: httpMethod
]

{ #category : #'accessing request' }
ZnClient >> multiPartFormDataEntity [
	"Initialize the current request to have an multipart/form-data entity.
	Return the existing entity if there is one. 
	Fail if another type was already set."

	request hasEntity
		ifTrue: [
			(request entity contentType matches: ZnMimeType multiPartFormData)
				ifFalse: [ self error: 'An incompatible entity was already set for the request' ] ]
		ifFalse: [
			request entity: ZnMultiPartFormDataEntity new ].
	^ request entity
]

{ #category : #private }
ZnClient >> newConnectionTo: url [
	| initialMilliseconds |
	self signalHTTPProgress: 'Connecting to ',  url authority.
	initialMilliseconds := Time millisecondClockValue.
	(connection notNil and: [ connection isConnected ]) 
		ifTrue: [ connection close ].
	connection := ZnNetworkingUtils socketStreamToUrl: url.
	url hasSecureScheme
		ifTrue: [ self setupTLSTo: url ].
	self logConnectionEstablishedTo: url started: initialMilliseconds
]

{ #category : #'private logging' }
ZnClient >> newLogEvent: logEventClass [
	^ logEventClass new
		clientId: self clientId;
		yourself
]

{ #category : #options }
ZnClient >> numberOfRetries [
	"Return how many times I will retry a failed request.
	Defaults to 0."
	
	^ self optionAt: #numberOfRetries ifAbsent: [ 0 ]
]

{ #category : #options }
ZnClient >> numberOfRetries: count [
	"Set the number of times I will retry a failed request to count."
	
	self optionAt: #numberOfRetries put: count
]

{ #category : #accessing }
ZnClient >> optionAt: key ifAbsent: block [
	"Return my option/settings stored under key.
	Execute block if I have no such option/setting.
	This is a generic interface, see my options protocol for specific usages."

	options ifNil: [ ^ block value ].
	^ options at: key ifAbsent: block
]

{ #category : #accessing }
ZnClient >> optionAt: key ifAbsentPut: block [
	"Return my option/settings stored under key.
	If I have no such option/setting, store the result of evaluating block as new value and return it.
	This is a generic interface, see my options protocol for specific usages."

	options ifNil: [ options := Dictionary new ].
	^ options at: key ifAbsentPut: block
]

{ #category : #accessing }
ZnClient >> optionAt: key put: value [
	"Set my option/setting identified by key to be value.
	This is a generic interface, see my options protocol for specific usages."
	
	options ifNil: [ options := Dictionary new ].
	options at: key put: value
]

{ #category : #operations }
ZnClient >> options [
	"Execute an HTTP OPTIONS on the request set up and return the response #contents."
	
	^ self 
		method: #OPTIONS; 
		execute
]

{ #category : #convenience }
ZnClient >> options: url [
	"Execute an HTTP OPTIONS request on url and return the response #contents."

	^ self 
		url: url; 
		options
]

{ #category : #operations }
ZnClient >> patch [
	"Execute a HTTP PATCH on the request set up and return the response #contents."

	^ self 
		method: #PATCH; 
		execute
]

{ #category : #convenience }
ZnClient >> patch: url contents: object [
	"Execute an HTTP PATCH request on url with object as request #contents:
	and return the response #contents."

	^ self 
		url: url; 
		contents: object; 
		patch
]

{ #category : #'accessing request' }
ZnClient >> path: pathString [
	"Set the path of the current request to pathString.
	The argument will be merged with of the current/previous request."
	
	self url: pathString
]

{ #category : #'accessing request' }
ZnClient >> port: portNumber [
	"Set the port of the current request to portNumber."
	
	self url: (ZnUrl new port: portNumber; yourself)
]

{ #category : #operations }
ZnClient >> post [
	"Execute a HTTP POST on the request set up and return the response #contents."

	^ self 
		method: #POST; 
		execute
]

{ #category : #convenience }
ZnClient >> post: url contents: object [
	"Execute an HTTP POST request on url with object as request #contents:
	and return the response #contents."

	^ self 
		url: url; 
		contents: object; 
		post
]

{ #category : #'private protocol' }
ZnClient >> prepareNextRequest [
	"The request could be modified destructively in the next run,
	but the request can also be part of a log event, hence copy it"
	request := request copy	

]

{ #category : #'private protocol' }
ZnClient >> prepareRedirect [
	"We implement the common behavior that POST/PUT redirects turn into GET request
	and that the body data is dropped, except for a 307 or 308 where this is explicitly forbidden.
	Note the cookie handling too."

	| targetUrl |
	targetUrl := self redirectUrl.
	self logRedirect: targetUrl.
	self receiveCookies.
	self prepareNextRequest.
	(#(307 308) includes: response code)
		ifFalse: [  
			(#(POST PUT) includes: request method)
				ifTrue: [ self method: #GET ].
			request hasEntity
				ifTrue: [ request resetEntity: nil ] ].
	self url: targetUrl.
	self sendCookies
]

{ #category : #'private protocol' }
ZnClient >> prepareRequest [
	self isOneShot 
		ifTrue: [ request setConnectionClose ].
	request hasEntity
		ifTrue: [ 
			"This is necessary because entities are often modified after first being added empty,
			and a wrong content length is cached at the request level"
			request resetEntity: request entity ].
	self sendCookies.
	self prepareRequestHook 
		ifNotNil: [ :hook | hook value: request ]
]

{ #category : #options }
ZnClient >> prepareRequest: hook [
	"Set the current request preparation hook, an object conforming 
	to the #value: protocol that gets the final chance to change a request 
	right before it gets executed. Defaults to nil.
	Typically used to sign requests. See also #prepareRequestHook "

	^ self optionAt: #prepareRequestHook put: hook
]

{ #category : #options }
ZnClient >> prepareRequestHook [
	"Return the current request preparation hook, an object conforming 
	to the #value: protocol that gets the final chance to change a request 
	right before it gets executed. Defaults to nil.
	Typically used to sign requests. See also #prepareRequest: "

	^ self optionAt: #prepareRequestHook ifAbsent: [ nil ]
]

{ #category : #operations }
ZnClient >> put [
	"Execute a HTTP PUT on the request set up and return the response #contents."

	^ self 
		method: #PUT; 
		execute
]

{ #category : #convenience }
ZnClient >> put: url contents: object [
	"Execute an HTTP PUT request on url with object as request #contents:
	and return the response #contents."

	^ self 
		url: url; 
		contents: object; 
		put
]

{ #category : #'accessing request' }
ZnClient >> queryAddAll: keyedCollection [
	"Add all key -> value elements of keyedCollection 
	as HTTP query parameters to the current request.
	Note that #queryAt:add: is used to preserve duplicate keys."

	keyedCollection keysAndValuesDo: [ :key :value |
		self queryAt: key add: value ]
]

{ #category : #'accessing request' }
ZnClient >> queryAt: key add: value [ 
	"Modify the receiver by adding a query variable key=value.
	If key already exists, value is added to it."

	request url 
		ifNil: [ request url: (ZnUrl new queryAt: key add: value; yourself) ]
		ifNotNil: [ request url queryAt: key add: value ]  
]

{ #category : #'accessing request' }
ZnClient >> queryAt: key put: value [
	"Add key equals value as an HTTP query parameter to the current request.
	If the key already exists, its value is overwritten."
	
	request url 
		ifNil: [ request url: (ZnUrl new queryAt: key put: value; yourself) ]
		ifNotNil: [ request url queryAt: key put: value ]  
]

{ #category : #'private protocol' }
ZnClient >> readResponse [
	self signalHTTPProgress: 'Reading response'.
	self withMaximumNumberOfDictionaryEntriesDo: [
		self withDefaultCharacterEncoderDo: [
			response := (#(HEAD CONNECT) includes: request method)
				ifTrue: [ ZnResponse readHeaderFrom: connection ]
				ifFalse: [ 
					self streaming
						ifTrue: [ ZnResponse readStreamingFrom: connection ]
						ifFalse: [ 
							self withMaximumEntitySizeDo: [ self reader value: connection ] ] ] ] ].
	lastUsed := Time totalSeconds
]

{ #category : #options }
ZnClient >> reader [
	"Return a block that when given a stream reads a response from it."
	
	^ self optionAt: #reader ifAbsentPut: [ [ :stream | ZnResponse readFrom: stream ] ]
]

{ #category : #options }
ZnClient >> reader: block [
	"Customize how responses are read from a stream, see #reader, see #beBinary"
	
	self optionAt: #reader put: block
]

{ #category : #private }
ZnClient >> receiveCookies [
	response cookies do: [ :each |
		each defaultDomain: request url host.
		self session cookieJar add: each ]
]

{ #category : #'private protocol' }
ZnClient >> redirectUrl [
	"We support relative locations although they are not the standard (http://en.wikipedia.org/wiki/HTTP_location)"

	| redirectUrl |
	redirectUrl := response location asZnUrl.
	^ redirectUrl isAbsolute
		ifTrue: [ redirectUrl ]
		ifFalse: [ redirectUrl inContextOf: request url ]
]

{ #category : #'accessing request' }
ZnClient >> request [
	"Return the current request that I am building,
	or the previous request that I executed."
	
	^ request 
]

{ #category : #'accessing request' }
ZnClient >> request: aRequest [
	"Replace my current request with aRequest.
	This is a low level operation only needed in special cases."
	
	self validateConnectionForHost: aRequest url host port: aRequest url port.
	request := aRequest
]

{ #category : #accessing }
ZnClient >> resetCookies [
	session ifNotNil: [ session resetCookies ]
]

{ #category : #'accessing request' }
ZnClient >> resetEntity [
	"Make sure that the current request does not hold an entity from a previous request.
	This is sometimes necessary after a POST or PUT. This method will be called automatically
	by #method: for certain requests."
	
	(request notNil and: [ request hasEntity ])
		ifTrue: [ request resetEntity: nil ]
]

{ #category : #'accessing response' }
ZnClient >> response [ 
	"Answer the last response, assuming the current request was executed."

	^ response 
]

{ #category : #options }
ZnClient >> retryDelay [
	"Return the number of seconds I will delay retrying a failed request,
	provided I do retry requests. Defaults to 1 seconds."
	
	^ self optionAt: #retryDelay ifAbsent: [ 1 ]
]

{ #category : #options }
ZnClient >> retryDelay: seconds [
	"Set the delay before I will delay retrying a failed request to seconds,
	provided I do retry requests."

	self optionAt: #retryDelay put: seconds
]

{ #category : #private }
ZnClient >> retryExceptionSet [
	^ NetworkError, ZnParseError, ZnCharacterEncodingError, ZnUnknownScheme, ZnPortNotANumber
]

{ #category : #private }
ZnClient >> sendCookies [
	| cookies cookiesString |
	session ifNil: [ ^ self ].
	cookies := (session cookieJar cookiesForUrl: request url).
	cookies isEmpty ifTrue: [ ^ self ]. 
	cookiesString := String streamContents: [ :stream |
		cookies
			do: [ :each | each writeNameAndValueOn: stream ]
			separatedBy: [ stream nextPutAll: '; ' ] ].
	request setCookie: cookiesString
]

{ #category : #accessing }
ZnClient >> session [
	"Return the current HTTP session object that I am using.
	This session object holds cookies among other things."

	^ session ifNil: [ session := ZnUserAgentSession new ]
]

{ #category : #'accessing request' }
ZnClient >> setAcceptEncodingGzip [
	"Give the server the hint that we accept gzip content encoding.
	When the server complies, important bandwidth reductions are possible."

	self request setAcceptEncodingGzip
]

{ #category : #'accessing request' }
ZnClient >> setBasicAuthenticationUsername: username password: password [
	"Set the username/password for basic authenticationfor the current request."
	
	self request setBasicAuthenticationUsername: username password: password

]

{ #category : #'accessing request' }
ZnClient >> setIfModifiedSince: reference [
	"Tell the server that we are only interested in the currently requested URI
	if it was modified since reference, a DateAndTime.
	If the URI was not modified since reference, the response will be #isNotModified."

	self request setIfModifiedSince: reference
]

{ #category : #private }
ZnClient >> setupTLSTo: url [
	(ZnNetworkingUtils shouldProxyUrl: url)
		ifTrue: [ | originalRequest |
			"http://www.ietf.org/rfc/rfc2817.txt (section 5)"
			"https://en.wikipedia.org/wiki/HTTP_tunnel#HTTP_CONNECT_tunneling"
			originalRequest := request copy.
			request entity: nil. "to prevent #resetEntity from being called, resulting in #close of the shared entity"
			self method: #CONNECT; writeRequest; readResponse.
			response isSuccess ifFalse: [ self error: 'Failed to CONNECT to proxy for TLS/SSL' ].
			connection := ZnNetworkingUtils secureSocketStreamOn: connection socket.
			request := originalRequest ].
	
	connection sslSession certificateName: self certificate.
	
	[ connection sslSession serverName: url host ] 
		on: PrimitiveFailed 
		do: [ :error | "log it?" ].
		
	connection connect
]

{ #category : #private }
ZnClient >> signalHTTPProgress: message [
	self signalProgress
		ifTrue: [ HTTPProgress signal: message ]
]

{ #category : #options }
ZnClient >> signalProgress [
	"Return true if I signal HTTPProgress notifications during execution."
	
	^ self optionAt: #signalProgress ifAbsent: [ false ]
]

{ #category : #options }
ZnClient >> signalProgress: boolean [
	"Set whether I will signal HTTPProgress notifications during execution to boolean."
	
	self optionAt: #signalProgress put: boolean
]

{ #category : #options }
ZnClient >> streaming [
	"Return whether I will return streaming response objects (see ZnStreamingEntity).
	If true, my users are responsible for actually reading the incoming response entity data stream."
	
	^ self optionAt: #streaming ifAbsent: [ false ]
]

{ #category : #options }
ZnClient >> streaming: boolean [
	"Set whether I will return streaming response objects (see ZnStreamingEntity) to boolean.
	If true, my users are responsible for actually reading the incoming response entity data stream.
	I cannot do streaming when #isOneShot is true because the stream would be closed too soon."

	(boolean and: [ self isOneShot ])
		ifTrue: [ self error: 'I cannot do streaming when #isOneShot is true.' ].
	self optionAt: #streaming put: boolean
]

{ #category : #initialization }
ZnClient >> systemPolicy [
	self 
		enforceHttpSuccess: true;
		enforceAcceptContentType: true;
		numberOfRetries: 2
]

{ #category : #options }
ZnClient >> timeout [
	"Return the timeout in seconds for network socket stream operations
	like connecting, reading and writing. Defaults to a global setting."
	
	^ self optionAt: #timeout ifAbsent: [ ZnNetworkingUtils defaultSocketStreamTimeout ]
]

{ #category : #options }
ZnClient >> timeout: seconds [
	"Set the timeout for network socket stream operations
	like connecting, reading and writing to seconds."

	self optionAt: #timeout put: seconds
]

{ #category : #convenience }
ZnClient >> uploadEntityFrom: path [
	"Set up a ZnStreamingEntity from the binary contents of the file at path.
	The path extension is used to guess the content-type,
	can be overridden using #contentType: if necessary.
	Use the file's length to set the content-length.
	You can then use #put or #post to actually do an upload."
	
	| stream type length |
	stream := ZnFileSystemUtils binaryFileReadStreamFor: path.
	type := ZnMimeType forFilenameExtension: (ZnFileSystemUtils extensionFor: path).
	length := ZnFileSystemUtils fileSizeFor: path.
	self entity: (ZnStreamingEntity readFrom: stream usingType: type andLength: length)
]

{ #category : #'accessing request' }
ZnClient >> url: urlObject [
	"Modify the URL of the current request object to urlObject.
	This is done using ZnUrl>>#inContextOf: so that either scheme, 
	host, port or path are set and merged.
	Default the URL using ZnUrl>>#asZnUrlWithDefaults to fall back
	to http or the default http/https ports."
	
	| url |
	url := urlObject asZnUrl.
	request url
		ifNotNil: [ url := url inContextOf: request url ].  
	url := url asZnUrlWithDefaults.  
	self validateConnectionForHost: url host port: url port.   
	request url: url.
	url hasUsername & url hasPassword
		ifTrue: [ self setBasicAuthenticationUsername: url username password: url password ]
]

{ #category : #'accessing request' }
ZnClient >> username: username password: password [
	"Set the username/password for basic authenticationfor the current request."
	
	self setBasicAuthenticationUsername: username password: password

]

{ #category : #private }
ZnClient >> validateConnectionForHost: host port: port [
	(connection isNil or: [ request url isNil ]) 
		ifTrue: [ ^ self ].
	(request url host = host and: [ request url port = port ])
		ifFalse: [ self close ]  
]

{ #category : #private }
ZnClient >> withDefaultCharacterEncoderDo: block [
	^ self defaultEncoder = ZnDefaultCharacterEncoder value
			ifTrue: block
			ifFalse: [ 
				ZnDefaultCharacterEncoder 
					value: self defaultEncoder  
					during: block ]
]

{ #category : #private }
ZnClient >> withMaximumEntitySizeDo: block [
	^ self maximumEntitySize = ZnMaximumEntitySize value
			ifTrue: block
			ifFalse: [ 
				ZnMaximumEntitySize 
					value: self maximumEntitySize  
					during: block ]
]

{ #category : #private }
ZnClient >> withMaximumNumberOfDictionaryEntriesDo: block [
	^ self maximumNumberOfDictionaryEntries = ZnMaximumNumberOfDictionaryEntries value
			ifTrue: block
			ifFalse: [ 
				ZnMaximumNumberOfDictionaryEntries 
					value: self maximumNumberOfDictionaryEntries 
					during: block ]
]

{ #category : #private }
ZnClient >> withProgressDo: block [
	^ ZnSignalProgress value
		ifNil: [
			ZnSignalProgress 
				value: self signalProgress 
				during: [ ^ block value ] ]
		ifNotNil: [ 
			block value ]
]

{ #category : #private }
ZnClient >> withTimeoutDo: block [
	^ ZnConnectionTimeout value
		ifNil: [
			ZnConnectionTimeout 
				value: self timeout 
				during: [ ^ block value ] ]
		ifNotNil: [ 
			block value ]
]

{ #category : #'private protocol' }
ZnClient >> writeRequest [
	self signalHTTPProgress: 'Writing request'.
	self withDefaultCharacterEncoderDo: [ request writeOn: connection ].
	connection flush
]
