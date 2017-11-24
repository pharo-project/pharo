"
I am ZnEntity, an abstract HTTP Entity.

An entity has a content type (mime-type), a content length (byte length) and a contents.

Entities can encode and decode themselves to and from binary streams.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnEntity,
	#superclass : #Object,
	#instVars : [
		'contentType',
		'contentLength'
	],
	#category : #'Zinc-HTTP-Core'
}

{ #category : #accessing }
ZnEntity class >> byteArrayEntityClass [
	^ ZnByteArrayEntity 
]

{ #category : #'instance creation' }
ZnEntity class >> bytes: bytes [
	^ self byteArrayEntityClass bytes: bytes
]

{ #category : #private }
ZnEntity class >> concreteSubclassForType: mimeType binary: forceBinary [
	"Answer the concrete ZnEntity subclass that handles mimeType"
	
	^ self allSubclasses 
		detect: [ :each | 
			each matches: mimeType ]
		ifNone: [ 
			(mimeType isBinary or: [ forceBinary ])
				ifTrue: [ self byteArrayEntityClass ] 
				ifFalse: [ self stringEntityClass ] ]

]

{ #category : #testing }
ZnEntity class >> designatedMimeType [
	"Answer the mime type we are designated to handle.
	See #concreteSubclassForType: "
	
	self subclassResponsibility 
]

{ #category : #'instance creation' }
ZnEntity class >> html: html [
	^ self stringEntityClass html: html
]

{ #category : #'instance creation' }
ZnEntity class >> json: text [
	^ self stringEntityClass json: text
]

{ #category : #testing }
ZnEntity class >> matches: mimeType [
	"Return whether a given mimeType can be handled by instances of the receiver"
	
	^ mimeType matches: self designatedMimeType 
]

{ #category : #'instance creation' }
ZnEntity class >> new [
	^ super new initialize 
]

{ #category : #'instance creation' }
ZnEntity class >> readBinaryFrom: stream usingType: mimeType andLength: length [
	"Create and return a new buffered entity of mimeType and length from stream.
	The concrete subclass depends on mimeType, but textual type will remain binary"
	
	| newEntity |
	newEntity := (self concreteSubclassForType: mimeType binary: true) 
						type: mimeType length: length.
	newEntity readBinaryFrom: stream.
	^ newEntity 

]

{ #category : #'instance creation' }
ZnEntity class >> readFrom: stream usingType: mimeType andLength: length [
	"Create and return a new buffered entity of mimeType and length from stream.
	The concrete subclass depends on mimeType"
	
	| newEntity |
	newEntity := (self concreteSubclassForType: mimeType binary: false) 
						type: mimeType length: length.
	newEntity readFrom: stream.
	^ newEntity 

]

{ #category : #accessing }
ZnEntity class >> stringEntityClass [
	^ ZnStringEntity
]

{ #category : #'instance creation' }
ZnEntity class >> text: text [
	^ self stringEntityClass text: text
]

{ #category : #'instance creation' }
ZnEntity class >> textCRLF: text [
	^ self text: text, String crlf
]

{ #category : #'instance creation' }
ZnEntity class >> type: type [
	"Create a new entity with a given mime type and (currently) undetermined length"
	
	^ self new
		contentType: type;
		yourself
]

{ #category : #'instance creation' }
ZnEntity class >> type: type length: length [
	"Create a new entity with a given mime type and length"
	
	^ self new
		contentType: type;
		contentLength: length;
		yourself
]

{ #category : #'instance creation' }
ZnEntity class >> with: object [
	"Create a new entity holding object.
	Currently works for strings and byte arrays.
	You will probably want to set the mime type to something more specific"
	
	^ object isString 
		ifTrue: [ self text: object ] 
		ifFalse: [ self bytes: object ]
]

{ #category : #'instance creation' }
ZnEntity class >> with: object type: type [
	"Create a new entity holding object.
	Currently works for strings and byte arrays.
	The entity is set to mime type"
	
	^ (self with: object)
		contentType: type;
		yourself
]

{ #category : #'instance creation' }
ZnEntity class >> xml: text [
	^ self stringEntityClass xml: text
]

{ #category : #comparing }
ZnEntity >> = other [
	self class = other class ifFalse: [ ^ false ].
	^ self contentType = other contentType and: [ self contentLength = other contentLength ]
]

{ #category : #converting }
ZnEntity >> asByteArray [
	"Return the ByteArray that is the binary, encoded representation of the receiver"
	
	^ ByteArray streamContents: [ :stream | self writeOn: stream ]
]

{ #category : #'initialize-release' }
ZnEntity >> close [
]

{ #category : #accessing }
ZnEntity >> contentLength [ 
	"Content length can be nil when it is (currently) unknown."
	
	^ contentLength
]

{ #category : #accessing }
ZnEntity >> contentLength: integer [
	contentLength := integer
]

{ #category : #accessing }
ZnEntity >> contentType [
	^ contentType 
]

{ #category : #accessing }
ZnEntity >> contentType: object [
	"We only allow assignment compatible with our designated mime type"
	
	| newType |
	newType := object asZnMimeType.
	contentType = newType 
		ifTrue: [ ^ self ]
		ifFalse: [ 
			(self class designatedMimeType isNil or: [ self class matches: newType ])
				ifTrue: [ contentType := newType ] ]
]

{ #category : #accessing }
ZnEntity >> contents [
	self subclassResponsibility
]

{ #category : #testing }
ZnEntity >> hasContentLength [
	^ contentLength notNil
]

{ #category : #testing }
ZnEntity >> hasContentType [
	^ contentType notNil
]

{ #category : #comparing }
ZnEntity >> hash [
	^ self contentType hash bitXor: self contentLength hash
]

{ #category : #testing }
ZnEntity >> isEmpty [ 
	self subclassResponsibility 
]

{ #category : #printing }
ZnEntity >> printContentTypeAndLengthOn: stream [
	self hasContentType ifTrue: [ 
		stream print: self contentType; space ].
	self hasContentLength ifTrue: [
		stream print: self contentLength; nextPut: $B ]
]

{ #category : #printing }
ZnEntity >> printContentsOn: stream [
	self printContentTypeAndLengthOn: stream
]

{ #category : #printing }
ZnEntity >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(.
	self printContentsOn: stream.
	stream nextPut: $)
]

{ #category : #'initialize-release' }
ZnEntity >> readBinaryFrom: stream [
	self readFrom: stream
]

{ #category : #'initialize-release' }
ZnEntity >> readFrom: stream [
	self subclassResponsibility 
]

{ #category : #writing }
ZnEntity >> writeOn: stream [
	self subclassResponsibility 
]
