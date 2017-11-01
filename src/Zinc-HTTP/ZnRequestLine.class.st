"
I am an HTTP Request Line.
I consist of an HTTP method, an URI and an HTTP protocol version.
I can be used for generating and parsing.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnRequestLine,
	#superclass : #Object,
	#instVars : [
		'method',
		'uri',
		'version'
	],
	#category : 'Zinc-HTTP-Core'
}

{ #category : #'instance creation' }
ZnRequestLine class >> empty [
	^ self new
		version: ZnConstants defaultHTTPVersion;
		yourself 
]

{ #category : #'instance creation' }
ZnRequestLine class >> method: method uri: uri [
	^ self new
		method: method;
		uri: uri;
		version: ZnConstants defaultHTTPVersion;
		yourself 
]

{ #category : #'instance creation' }
ZnRequestLine class >> readFrom: stream [
	^ self new
		readFrom: stream;
		yourself
]

{ #category : #comparing }
ZnRequestLine >> = other [
	self class = other class ifFalse: [ ^ false ].
	^ self method = other method
		and: [ self uri asRelativeUrl = other uri asRelativeUrl
			and: [ self version = other version ] ]
]

{ #category : #comparing }
ZnRequestLine >> hash [
	^ (self method hash bitXor: self uri hash) bitXor: self version hash
]

{ #category : #testing }
ZnRequestLine >> isHttp10 [
	^ self version = 'HTTP/1.0'
]

{ #category : #testing }
ZnRequestLine >> isHttp11 [
	^ self version = 'HTTP/1.1'
]

{ #category : #accessing }
ZnRequestLine >> method [
	^ method
]

{ #category : #accessing }
ZnRequestLine >> method: string [
	method := ZnConstants knownHTTPMethods 
		detect: [ :each | each = string ] 
		ifNone: [ (ZnUnknownHttpMethod method: string) signal ] 
]

{ #category : #copying }
ZnRequestLine >> postCopy [
	uri := uri copy
]

{ #category : #printing }
ZnRequestLine >> printMethodAndUriOn: stream [
	(self method notNil and: [ self uri notNil ])
		ifFalse: [ ^ self ].
	stream
		nextPutAll: self method;
		space.
	self uri printPathQueryFragmentOn: stream
]

{ #category : #printing }
ZnRequestLine >> printOn: stream [
	super printOn: stream.
	(self method notNil and: [ self uri notNil ])
		ifFalse: [ ^ self ].
	stream nextPut: $(.
	self printMethodAndUriOn: stream.
	stream nextPut: $)
]

{ #category : #initialize-release }
ZnRequestLine >> readFrom: stream [
	| line lineStream |
	line := (ZnLineReader on: stream) nextLine.
	lineStream := line readStream.
	self method: (lineStream upTo: Character space).
	self uri: (lineStream upTo: Character space).
	self version: (lineStream upToEnd)
]

{ #category : #accessing }
ZnRequestLine >> uri [
	^ uri
]

{ #category : #accessing }
ZnRequestLine >> uri: anURI [
	uri := anURI asZnUrl.
	uri enforceKnownScheme
]

{ #category : #accessing }
ZnRequestLine >> uriPathQueryFragment [
	^ String streamContents: [ :stream | 
			self uri printPathQueryFragmentOn: stream ]
]

{ #category : #accessing }
ZnRequestLine >> version [
	^ version
]

{ #category : #accessing }
ZnRequestLine >> version: string [
	version := ZnConstants knownHTTPVersions 
		detect: [ :each | each = string ] 
		ifNone: [ (ZnUnknownHttpVersion version: string) signal ] 
]

{ #category : #writing }
ZnRequestLine >> writeOn: stream [
	| writeStream |
	writeStream := ZnBivalentWriteStream on: stream.
	writeStream nextPutAll: self method; space.
	(ZnNetworkingUtils shouldProxyUrl: self uri)
		ifTrue: [ 
			self uri hasSecureScheme 
				ifTrue: [
					"See also ZnClient>>#setupTLSTo:"
					method = #CONNECT
						ifTrue: [ writeStream nextPutAll: uri authorityWithPort ]
						ifFalse: [ self uri printPathQueryFragmentOn: writeStream ] ]
				ifFalse: [ self uri printOn: writeStream ] ]
		ifFalse: [ self uri printPathQueryFragmentOn: writeStream ].
	writeStream space; nextPutAll: self version; nextPutAll: String crlf 
]
