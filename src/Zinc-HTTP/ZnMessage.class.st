"
I am ZnMessage, an abstract class representing an HTTP Message.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnMessage,
	#superclass : #Object,
	#instVars : [
		'headers',
		'entity'
	],
	#category : #'Zinc-HTTP-Core'
}

{ #category : #'instance creation' }
ZnMessage class >> readBinaryFrom: stream [
	^ self new
		readBinaryFrom: stream;
		yourself
]

{ #category : #'instance creation' }
ZnMessage class >> readFrom: stream [
	^ self new
		readFrom: stream;
		yourself
]

{ #category : #'instance creation' }
ZnMessage class >> readHeaderFrom: stream [
	^ self new
		readHeaderFrom: stream;
		yourself
]

{ #category : #'instance creation' }
ZnMessage class >> readStreamingFrom: stream [
	^ self new
		readStreamingFrom: stream;
		yourself
]

{ #category : #comparing }
ZnMessage >> = other [
	self class = other class ifFalse: [ ^ false ].
	^ self headers = other headers and: [ self entity = other entity ]
]

{ #category : #accessing }
ZnMessage >> clearEntity [
	"Set my entity to nil. 
	Unlike #resetEntity:, my content type and length remain unchanged."

	entity ifNotNil: [
		entity close.
		entity := nil ]
]

{ #category : #accessing }
ZnMessage >> contentLength [
	self hasEntity ifTrue: [ ^ self entity contentLength ].
	(self hasHeaders and: [ self headers hasContentLength ]) ifTrue: [ ^ self headers contentLength ].
	^ nil
]

{ #category : #accessing }
ZnMessage >> contentType [
	self hasEntity ifTrue: [ ^ self entity contentType ].
	(self hasHeaders and: [ self headers hasContentType ]) ifTrue: [ ^ self headers contentType ].
	^ nil
]

{ #category : #accessing }
ZnMessage >> contents [
	^ entity isNil ifTrue: [ nil ] ifFalse: [ entity contents ]
]

{ #category : #accessing }
ZnMessage >> cookies [
	self subclassResponsibility 
]

{ #category : #accessing }
ZnMessage >> entity [
	^ entity
]

{ #category : #accessing }
ZnMessage >> entity: object [
	"Set my entity to object. Unless I already am describing an entity using my content type and length
	set my content type and length as defined by object."

	entity := object.
	self headers isDescribingEntity ifFalse: [
		self headers acceptEntityDescription: object ]
]

{ #category : #private }
ZnMessage >> entityReaderOn: stream [
	^ ZnEntityReader new
		headers: self headers;
		stream: stream;
		yourself
]

{ #category : #private }
ZnMessage >> entityWriterOn: stream [
	^ ZnEntityWriter new
		headers: self headers;
		stream: stream;
		yourself
]

{ #category : #testing }
ZnMessage >> hasEntity [
	^ self entity notNil
]

{ #category : #testing }
ZnMessage >> hasHeaders [
	^ headers notNil and: [ self headers isEmpty not ]
]

{ #category : #testing }
ZnMessage >> hasSession [
	"Return if there currently is a server session.
	This only returns a value during #handleRequest:"

	^ ZnCurrentServerSession value notNil
]

{ #category : #comparing }
ZnMessage >> hash [
	^ self headers hash bitXor: self entity hash
]

{ #category : #accessing }
ZnMessage >> headers [
	headers isNil ifTrue: [ headers := ZnHeaders new ].
	^ headers
]

{ #category : #accessing }
ZnMessage >> headers: object [
	headers := object
]

{ #category : #enumerating }
ZnMessage >> headersDo: twoArgumentBlock [
	self hasHeaders
		ifTrue: [ self headers headersDo: twoArgumentBlock ]
]

{ #category : #testing }
ZnMessage >> isConnectionClose [
	| value |
	value := self headers at: 'Connection' ifAbsent: [ ^ false ].
	^ value sameAs: 'close'
]

{ #category : #testing }
ZnMessage >> isConnectionKeepAlive [
	| value |
	value := self headers at: 'Connection' ifAbsent: [ ^ false ].
	^ value sameAs: 'keep-alive'
]

{ #category : #copying }
ZnMessage >> postCopy [
	headers := headers copy.
	"Note that we don't copy the entity, see also #resetEntity: and ZnClient>>#resetEntity"
]

{ #category : #'initialize-release' }
ZnMessage >> readBinaryFrom: stream [
	| entityReader |
	self readHeaderFrom: stream.
	(entityReader := self entityReaderOn: stream)
		binary.
	self entity: entityReader readEntity
]

{ #category : #'initialize-release' }
ZnMessage >> readEntityFrom: stream [
	self entity: (self entityReaderOn: stream) readEntity
	
]

{ #category : #'initialize-release' }
ZnMessage >> readFrom: stream [
	self readHeaderFrom: stream.
	self readEntityFrom: stream
	
]

{ #category : #'initialize-release' }
ZnMessage >> readHeaderFrom: stream [
	self headers: (ZnHeaders readFrom: stream)
	
]

{ #category : #'initialize-release' }
ZnMessage >> readStreamingFrom: stream [
	| entityReader |
	self readHeaderFrom: stream.
	(entityReader := self entityReaderOn: stream)
		streaming.
	self entity: entityReader readEntity
	
]

{ #category : #accessing }
ZnMessage >> resetEntity: object [
	"Set my entity to object. Always set my content type and length as defined by object,
	even if I am already describing an entity using my content type and length.
	See also: #clearEntity"

	(entity ~= object and: [ entity notNil ]) 
		ifTrue: [ entity close ].
	entity := object.
	self headers acceptEntityDescription: object
]

{ #category : #accessing }
ZnMessage >> server [
	"Return the current server.
	This only returns a value during #handleRequest:"

	^ ZnCurrentServer value
]

{ #category : #accessing }
ZnMessage >> session [
	"Return the current server session.
	This only returns a value during #handleRequest:"

	^ ZnCurrentServerSession value
]

{ #category : #accessing }
ZnMessage >> setConnectionClose [
	self headers at: 'Connection' put: 'close'
]

{ #category : #accessing }
ZnMessage >> setConnectionKeepAlive [
	self headers at: 'Connection' put: 'keep-alive'
]

{ #category : #accessing }
ZnMessage >> setContentType: mimeType [
	"Explicitly set my Content-Type to mimeType.
	I normally do this automatically by taking the #contentType from my entity."
	
	self headers contentType: mimeType
]

{ #category : #testing }
ZnMessage >> wantsConnectionClose [
	"Return if the HTTP protocol should close the connection after processing the receiver"
	
	^ false
]

{ #category : #writing }
ZnMessage >> writeOn: stream [
	| bivalentWriteStream |
	bivalentWriteStream := ZnBivalentWriteStream on: stream.
	self headers writeOn: bivalentWriteStream.
	bivalentWriteStream nextPutAll: String crlf.
	self hasEntity ifTrue: [ 
		(self entityWriterOn: bivalentWriteStream) writeEntity: self entity ]
]

{ #category : #writing }
ZnMessage >> writeToTranscript [
	"Write the receiver on the Transcript like when sent or received over the network.
	Useful for debugging, obviously"

	self
		trace: (String streamContents: [ :stream | self writeOn: stream ])
]
