"
I am ZnMultiPartFormDataEntity, a concrete HTTP Entity 
holding multi part form data that is encoded according to specific rules.
I am a ZnEntity.

Acknowledgement: some code borrowed from AJP.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnMultiPartFormDataEntity,
	#superclass : #ZnEntity,
	#instVars : [
		'parts',
		'representation'
	],
	#category : 'Zinc-HTTP-Core'
}

{ #category : #testing }
ZnMultiPartFormDataEntity class >> designatedMimeType [
	^ ZnMimeType multiPartFormData  
]

{ #category : #testing }
ZnMultiPartFormDataEntity class >> matches: mimeType [
	^ mimeType matches: 'multipart/*' asZnMimeType
]

{ #category : #accessing }
ZnMultiPartFormDataEntity >> addPart: mimePart [
	self invalidateRepresentation.
	self parts add: mimePart
]

{ #category : #private }
ZnMultiPartFormDataEntity >> computeRepresentation [
	representation := ByteArray streamContents: [ :stream | 
		self writeRepresentationOn: (ZnBivalentWriteStream on: stream) ].
	contentLength := representation size
]

{ #category : #accessing }
ZnMultiPartFormDataEntity >> contentLength [
	contentLength isNil ifTrue: [ self computeRepresentation ].
	^ contentLength
]

{ #category : #accessing }
ZnMultiPartFormDataEntity >> contentType: object [
	"We only allow assignment compatible with our designated mime type.
	The main/sub must be equal but the parameters must be allowed to be different"
	
	| newType |
	newType := object asZnMimeType.
	(self class matches: newType)
		ifTrue: [ contentType := newType ]
]

{ #category : #accessing }
ZnMultiPartFormDataEntity >> contents [
	^ self parts
]

{ #category : #private }
ZnMultiPartFormDataEntity >> generateBoundary [
	^ String streamContents: [ :stream | | letters |
			stream nextPutAll: 'Boundary-Zn-'.
			letters := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
			8 timesRepeat: [ stream nextPut: letters atRandom ] ]
	
]

{ #category : #accessing }
ZnMultiPartFormDataEntity >> getBoundary [
	"Return the multipart/form-data MIME part boundary.
	This should really be there, set externally or internally.
	See #initialize and #mimeTypeWithBoundary"
	
	^ (self contentType parameterAt: 'boundary') withoutQuoting 
]

{ #category : #initialize-release }
ZnMultiPartFormDataEntity >> initialize [
	super initialize.
	self contentType: self mimeTypeWithBoundary  
	
]

{ #category : #private }
ZnMultiPartFormDataEntity >> invalidateRepresentation [
	representation := nil.
	contentLength := nil
]

{ #category : #testing }
ZnMultiPartFormDataEntity >> isEmpty [
	^ parts isNil or: [ parts isEmpty ]
]

{ #category : #private }
ZnMultiPartFormDataEntity >> mimeTypeWithBoundary [
	| mimeType |
	mimeType := self class designatedMimeType copy.
	mimeType parameterAt: 'boundary' put: self generateBoundary.
	^ mimeType
]

{ #category : #private }
ZnMultiPartFormDataEntity >> parse: bytes boundary: boundary binary: binary [
	| next start fullBoundary |
	fullBoundary := '--' asByteArray, boundary.
	"Sometimes there is whitespace in front of the first boundary"
	start := (bytes
		indexOfSubCollection: fullBoundary
		startingAt: 1) + 1.
	next := bytes
		indexOfSubCollection: fullBoundary
		startingAt: start 
		ifAbsent: [ bytes size ].
	[ next < (bytes size - 2) ] whileTrue: [ | partReadStream |
		partReadStream := ReadStream on: bytes from: start + 2 to: next - 3.
		self addPart: (ZnMimePart perform: (binary ifTrue: [ #readBinaryFrom: ] ifFalse: [ #readFrom: ]) with: partReadStream).
		start := next + fullBoundary size.
		next := bytes
			indexOfSubCollection: fullBoundary
			startingAt: start
			ifAbsent: [ bytes size ] ]
]

{ #category : #accessing }
ZnMultiPartFormDataEntity >> partNamed: fieldName [
	^ self partNamed: fieldName ifNone: [ self error: 'Cannot find part named ', fieldName asString ]
]

{ #category : #accessing }
ZnMultiPartFormDataEntity >> partNamed: fieldName ifNone: block [
	parts isNil ifTrue: [ block value ].
	^ self parts detect: [ :each | each fieldName = fieldName ] ifNone: block
]

{ #category : #accessing }
ZnMultiPartFormDataEntity >> parts [
	parts isNil ifTrue: [ parts := OrderedCollection new ].
	^ parts
]

{ #category : #enumerating }
ZnMultiPartFormDataEntity >> partsDo: block [
	parts isNil ifTrue: [ ^ self ].
	parts do: block  
]

{ #category : #printing }
ZnMultiPartFormDataEntity >> printContentsOn: stream [
	super printContentsOn: stream.
	self isEmpty
		ifTrue: [ ^ self ].
	stream space.
	self parts printElementsOn: stream
]

{ #category : #initialize-release }
ZnMultiPartFormDataEntity >> readBinaryFrom: stream [
	"Switch to streaming implementation later on"
	
	| bytes |
	"We have to use the contentLength instance variable instead of the accessor else a representation will be computed"
	contentLength isNil
		ifTrue: [ 
			bytes := ZnUtils readUpToEnd: stream limit: ZnMaximumEntitySize value ]
		ifFalse: [
			self contentLength > ZnMaximumEntitySize value ifTrue: [ ZnEntityTooLarge signal ].
			bytes := ByteArray ofSize: contentLength.
			stream next: contentLength into: bytes ].
	self parse: bytes boundary: self getBoundary asByteArray binary: true
]

{ #category : #initialize-release }
ZnMultiPartFormDataEntity >> readFrom: stream [
	"Switch to streaming implementation later on"
	
	| bytes |
	"We have to use the contentLength instance variable instead of the accessor else a representation will be computed"
	contentLength isNil
		ifTrue: [ 
			bytes := ZnUtils readUpToEnd: stream limit: ZnMaximumEntitySize value ]
		ifFalse: [
			self contentLength > ZnMaximumEntitySize value ifTrue: [ ZnEntityTooLarge signal ].
			bytes := ByteArray ofSize: contentLength.
			stream next: contentLength into: bytes ].
	self parse: bytes boundary: self getBoundary asByteArray binary: false
]

{ #category : #writing }
ZnMultiPartFormDataEntity >> writeOn: stream [
	"We always go via our representation because we need a bivalent stream"
	
	representation isNil ifTrue: [ self computeRepresentation ].
	ZnUtils nextPutAll: representation on: stream
]

{ #category : #writing }
ZnMultiPartFormDataEntity >> writeRepresentationOn: stream [
	| boundary |
	boundary := self getBoundary.
	self parts do: [ :each |
		stream nextPutAll: '--'; nextPutAll: boundary; nextPutAll: String crlf.
		each writeOn: stream.
		stream nextPutAll: String crlf ].
	stream nextPutAll: '--'; nextPutAll: boundary; nextPutAll: '--'; nextPutAll: String crlf

]
