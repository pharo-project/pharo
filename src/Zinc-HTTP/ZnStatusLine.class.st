"
I am an HTTP Status Line.
I consist of an HTTP protocol version, an integer HTTP status code and an HTTP reason string.
I can be used for generating and parsing.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnStatusLine,
	#superclass : #Object,
	#instVars : [
		'version',
		'code',
		'reason'
	],
	#category : #'Zinc-HTTP-Core'
}

{ #category : #'instance creation' }
ZnStatusLine class >> accepted [
	^ self code: 202
]

{ #category : #'instance creation' }
ZnStatusLine class >> badRequest [
	^ self code: 400
]

{ #category : #'instance creation' }
ZnStatusLine class >> code: code [
	^ self new
		code: code;
		version: ZnConstants defaultHTTPVersion;
		yourself
]

{ #category : #'instance creation' }
ZnStatusLine class >> created [
	^ self code: 201
]

{ #category : #'instance creation' }
ZnStatusLine class >> forbidden [
	^ self code: 403
]

{ #category : #'instance creation' }
ZnStatusLine class >> found [
	^ self code: 302
]

{ #category : #'instance creation' }
ZnStatusLine class >> internalServerError [
	^ self code: 500
]

{ #category : #'instance creation' }
ZnStatusLine class >> methodNotAllowed [
	^ self code: 405
]

{ #category : #'instance creation' }
ZnStatusLine class >> movedPermanently [
	^ self code: 301
]

{ #category : #'instance creation' }
ZnStatusLine class >> noContent [
	^ self code: 204
]

{ #category : #'instance creation' }
ZnStatusLine class >> notFound [
	^ self code: 404
]

{ #category : #'instance creation' }
ZnStatusLine class >> notImplemented [
	^ self code: 501
]

{ #category : #'instance creation' }
ZnStatusLine class >> notModified [
	^ self code: 304
]

{ #category : #'instance creation' }
ZnStatusLine class >> ok [
	^ self code: 200
]

{ #category : #'instance creation' }
ZnStatusLine class >> readFrom: stream [
	^ self new
		readFrom: stream;
		yourself
]

{ #category : #'instance creation' }
ZnStatusLine class >> redirect [
	^ self code: 302
]

{ #category : #'instance creation' }
ZnStatusLine class >> seeOther [
	^ self code: 303
]

{ #category : #'instance creation' }
ZnStatusLine class >> unauthorized [
	^ self code: 401
]

{ #category : #comparing }
ZnStatusLine >> = other [
	self class = other class ifFalse: [ ^ false ].
	^ self code = other code 
		and: [ self version = other version
			and: [ self reason = other reason ] ]
]

{ #category : #accessing }
ZnStatusLine >> code [
	^ code
]

{ #category : #accessing }
ZnStatusLine >> code: integer [
	(ZnConstants httpStatusCodes includesKey: integer) 
		ifFalse: [
			(integer between: 100 and: 599)
				ifFalse: [
					(ZnUnknownHttpStatusCode code: integer) signal ] ].
	code := integer
]

{ #category : #comparing }
ZnStatusLine >> hash [
	^ (self code hash bitXor: self version hash) bitXor: self reason hash
]

{ #category : #printing }
ZnStatusLine >> printCodeAndReasonOn: stream [
	self code
		ifNotNil: [
			stream print: self code; space; nextPutAll: self reason ]
]

{ #category : #printing }
ZnStatusLine >> printOn: stream [
	super printOn: stream.
	self code ifNil: [ ^ self ].
	stream nextPut: $(.
	self printCodeAndReasonOn: stream.
	stream nextPut: $)
]

{ #category : #'initialize-release' }
ZnStatusLine >> readFrom: stream [
	| line lineStream httpCode |
	line := (ZnLineReader on: stream) nextLine.
	lineStream := line readStream.
	self version: (lineStream upTo: Character space).
	httpCode := lineStream upTo: Character space.
	httpCode := Integer readFrom: httpCode ifFail: [ (ZnUnknownHttpStatusCode code: httpCode) signal ].
	self code: httpCode.
	self reason: (lineStream upToEnd)

]

{ #category : #accessing }
ZnStatusLine >> reason [
	reason ifNil: [ 
		reason := ZnConstants httpStatusCodes 
			at: self code 
			ifAbsent: [ 'Unknown Status Code' ] ].
	^ reason
]

{ #category : #accessing }
ZnStatusLine >> reason: string [
	reason := string
]

{ #category : #accessing }
ZnStatusLine >> version [
	^ version
]

{ #category : #accessing }
ZnStatusLine >> version: string [
	version := ZnConstants knownHTTPVersions 
		detect: [ :each | each = string ] 
		ifNone: [ (ZnUnknownHttpVersion version: string) signal ] 
]

{ #category : #writing }
ZnStatusLine >> writeOn: stream [
	| writeStream |
	writeStream := ZnBivalentWriteStream on: stream.
	writeStream nextPutAll: self version; space.
	self code printOn: writeStream.
	writeStream space; nextPutAll: self reason; nextPutAll: String crlf 
]
