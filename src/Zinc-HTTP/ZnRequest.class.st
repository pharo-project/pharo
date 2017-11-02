"
I am ZnRequest, representing an HTTP Request, 
consisting of a request line, headers and an optional entity (body).
I am a ZnMessage.
I can be used for generating and parsing.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnRequest,
	#superclass : #ZnMessage,
	#instVars : [
		'requestLine'
	],
	#category : 'Zinc-HTTP-Core'
}

{ #category : #'instance creation' }
ZnRequest class >> delete: urlObject [
	^ self method: #DELETE url: urlObject
]

{ #category : #'instance creation' }
ZnRequest class >> empty [
	^ self new
		requestLine: ZnRequestLine empty;
		headers: ZnHeaders defaultRequestHeaders;
		yourself
]

{ #category : #'instance creation' }
ZnRequest class >> get: urlObject [
	^ self method: #GET url: urlObject
]

{ #category : #'instance creation' }
ZnRequest class >> head: urlObject [
	^ self method: #HEAD url: urlObject
]

{ #category : #'instance creation' }
ZnRequest class >> method: method url: urlObject [
	| url |
	url := urlObject asZnUrl.
	^ self new
		requestLine: (ZnRequestLine method: method uri: url);
		headers: (ZnHeaders requestHeadersFor: url);
		yourself
]

{ #category : #'instance creation' }
ZnRequest class >> options: urlObject [
	^ self method: #OPTIONS url: urlObject
]

{ #category : #'instance creation' }
ZnRequest class >> patch: urlObject [
	^ self method: #PATCH url: urlObject
]

{ #category : #'instance creation' }
ZnRequest class >> post: urlObject [
	^ self method: #POST url: urlObject
]

{ #category : #'instance creation' }
ZnRequest class >> put: urlObject [
	^ self method: #PUT url: urlObject
]

{ #category : #comparing }
ZnRequest >> = other [
	^ super = other and: [ self requestLine = other requestLine ]
]

{ #category : #accessing }
ZnRequest >> absoluteUrl [
	^ self host 
		ifNil: [ self url ]
		ifNotNil: [ :hostUrl | self url inContextOf: hostUrl ]
]

{ #category : #accessing }
ZnRequest >> accept [
	^ self headers at: 'Accept'
]

{ #category : #testing }
ZnRequest >> acceptsEncodingGzip [
	| value |
	value := self headers at: 'Accept-Encoding' ifAbsent: [ ^ false ].
	^ value includesSubstring: 'gzip'
]

{ #category : #accessing }
ZnRequest >> authorization [
	^ self headers at: 'Authorization'
]

{ #category : #accessing }
ZnRequest >> basicAuthentication [
	| authorization separator |
	authorization := self authorization findTokens: ' '.
	(authorization size = 2 and: [ authorization first = 'Basic' ])
		ifFalse: [ ^ NotFound signal: 'Incomplete Basic Authentication' ].
	authorization := ZnUtils decodeBase64: authorization second.
	separator := authorization 
		indexOf: $: 
		ifAbsent: [ ^ NotFound signal: 'Incomplete Basic Authentication' ].
	^ { authorization first: separator - 1. authorization allButFirst: separator }
]

{ #category : #accessing }
ZnRequest >> cookies [
	| value cookies |
	value := self headers at: 'Cookie' ifAbsent: [ ^ #() ].
	cookies := value isString
		ifTrue: [ value findTokens: #($;) ]
		ifFalse: [ value ].
	^ cookies collect: [ :each | ZnCookie fromString: each ]
]

{ #category : #comparing }
ZnRequest >> hash [
	^ super hash bitXor: self requestLine hash
]

{ #category : #accessing }
ZnRequest >> host [
	| host scheme |
	host := self headers at: 'Host' ifAbsent: [ ^ nil ].
	scheme :=ZnCurrentServer value ifNil: [ #http ] ifNotNil: [ :server | server scheme ].
	^ ZnUrl fromString: host defaultScheme: scheme
]

{ #category : #testing }
ZnRequest >> isHttp10 [
	^ self requestLine isHttp10 
]

{ #category : #testing }
ZnRequest >> isHttp11 [
	^ self requestLine isHttp11
]

{ #category : #accessing }
ZnRequest >> mergedFields [
	"Return a new ZnMultiValueDictionary containing all query fields, if any,
	merged with all ZnApplicationFormUrlEncodedEntity fields, if any,
	preserving multi values for identical keys"

	| fields |
	fields := ZnMultiValueDictionary new.
	self uri hasQuery
		ifTrue: [ fields addAllMulti: self uri query ].
	(self contentType = ZnMimeType applicationFormUrlEncoded and: [ self hasEntity ])
		ifTrue: [ fields addAllMulti: self entity fields ].
	^ fields
]

{ #category : #accessing }
ZnRequest >> method [
	^ self requestLine method 
]

{ #category : #initialize-release }
ZnRequest >> method: method [
	self requestLine method: method
]

{ #category : #copying }
ZnRequest >> postCopy [
	super postCopy.
	requestLine := requestLine copy
]

{ #category : #printing }
ZnRequest >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(.
	self requestLine printMethodAndUriOn: stream.
	stream nextPut: $)
]

{ #category : #initialize-release }
ZnRequest >> readHeaderFrom: stream [
	self requestLine: (ZnRequestLine readFrom: stream).
	super readHeaderFrom: stream
	
]

{ #category : #accessing }
ZnRequest >> relativeUrl [
	^ self url asRelativeUrl 
]

{ #category : #accessing }
ZnRequest >> requestLine [
	^ requestLine 
]

{ #category : #accessing }
ZnRequest >> requestLine: object [
	requestLine := object
]

{ #category : #accessing }
ZnRequest >> session [
	"Return the current server session.
	If necessary a new session is created.
	This only returns a value during #handleRequest:"

	^ super session
		ifNil: [ 
			| session |
			session := self server sessionFor: self.
			ZnCurrentServerSession value: session.
			session ]
]

{ #category : #accessing }
ZnRequest >> setAccept: object [
	self headers at: 'Accept' put: object asString
]

{ #category : #accessing }
ZnRequest >> setAcceptEncodingGzip [
	self headers at: 'Accept-Encoding' put: 'gzip'
]

{ #category : #accessing }
ZnRequest >> setAuthorization: authorization [
	self headers at: 'Authorization'  put: authorization
]

{ #category : #accessing }
ZnRequest >> setBasicAuthenticationUsername: username password: password [
	(username isNil | password isNil)
		ifTrue: [
			self headers removeKey: 'Authorization' ifAbsent: []]
		ifFalse: [
			self setAuthorization: 'Basic ', (ZnUtils encodeBase64: (username, ':', password)) ]

]

{ #category : #accessing }
ZnRequest >> setCookie: cookie [
	self headers at: 'Cookie' put: cookie

]

{ #category : #accessing }
ZnRequest >> setIfModifiedSince: reference [
	self headers at: 'If-Modified-Since' put: (ZnUtils httpDate: reference)
]

{ #category : #accessing }
ZnRequest >> uri [
	^ self requestLine uri 
]

{ #category : #accessing }
ZnRequest >> url [
	^ self uri 
]

{ #category : #initialize-release }
ZnRequest >> url: url [
	self requestLine uri: url.
	self headers request: self url
]

{ #category : #testing }
ZnRequest >> wantsConnectionClose [
	"Return if the HTTP protocol should close the connection after processing the receiver.
	Overwritten to return true in case of the presense of an explicit connection close request header
	or the absense of a keep alive header when using the old HTTP 1.0 protocol."

	^ self isConnectionClose 
		or: [ self isHttp10 and: [ self isConnectionKeepAlive not ] ]
]

{ #category : #writing }
ZnRequest >> writeOn: stream [
	| bivalentWriteStream |
	bivalentWriteStream := ZnBivalentWriteStream on: stream.
	self requestLine writeOn: bivalentWriteStream.
	super writeOn: bivalentWriteStream
]
