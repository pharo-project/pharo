"
I am ZnMimePart.
I hold headers and an optional entity.
I am used in MultiPartMimeEncoding.

Acknowledgement: some code borrowed from AJP.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnMimePart,
	#superclass : #Object,
	#instVars : [
		'headers',
		'entity'
	],
	#category : #Zinc-HTTP-Core
}

{ #category : #'instance creation' }
ZnMimePart class >> fieldName: fieldName entity: entity [
	^ self new
		setContentDisposition: 'form-data;name="', fieldName, '"';
		entity: entity;
		yourself
]

{ #category : #'instance creation' }
ZnMimePart class >> fieldName: fieldName fileName: fileName entity: entity [
	"Pathnames are often silenty encoded using UTF-8,
	this is a no-op for ASCII, but will fail on Latin-1 and others"

	| encodedFileName |
	encodedFileName := fileName utf8Encoded asString.
	^ self new
		setContentDisposition: 'form-data;name="', fieldName, '";filename="', encodedFileName, '"';
		entity: entity;
		yourself
]

{ #category : #'instance creation' }
ZnMimePart class >> fieldName: fieldName fileNamed: fileName [
	| mimeType size fileStream entity baseName |
	mimeType := ZnMimeType forFilenameExtension: (ZnFileSystemUtils extensionFor: fileName).
	fileStream := ZnFileSystemUtils oldFileStreamFor: fileName.
	mimeType isBinary ifTrue: [ fileStream binary ].
	size := ZnFileSystemUtils fileSizeFor: fileName.
	(entity := ZnStreamingEntity type: mimeType length: size)
		stream: fileStream.
	baseName := ZnFileSystemUtils baseNameFor: fileName.
	^ self fieldName: fieldName fileName: baseName entity: entity
	
]

{ #category : #'instance creation' }
ZnMimePart class >> fieldName: fieldName value: fieldValue [
	^ self
		fieldName: fieldName
		entity: (ZnEntity with: fieldValue)
]

{ #category : #'instance creation' }
ZnMimePart class >> readBinaryFrom: stream [
	^ self new
		readBinaryFrom: stream;
		yourself
]

{ #category : #'instance creation' }
ZnMimePart class >> readFrom: stream [
	^ self new
		readFrom: stream;
		yourself
]

{ #category : #comparing }
ZnMimePart >> = other [
	self class = other class ifFalse: [ ^ false ].
	^ self headers = other headers and: [ self entity = other entity ]
]

{ #category : #accessing }
ZnMimePart >> contentDisposition [
	^ self headers at: 'Content-Disposition' ifAbsent: [ nil ]
]

{ #category : #private }
ZnMimePart >> contentDispositionValues [
	| contentDisposition values start semiColonIndex |
	(contentDisposition := self contentDisposition) isNil ifTrue: [ ^ #() ].
	values := OrderedCollection new: 3.
	start := 1.
	semiColonIndex := contentDisposition indexOf: $; startingAt: start ifAbsent: [ 0 ].
	[ semiColonIndex isZero ] whileFalse: [
		| value |
		value := contentDisposition copyFrom: start to: semiColonIndex - 1.
		values add: (ZnUtils trimString: value).
		start := semiColonIndex + 1.
		semiColonIndex := contentDisposition indexOf: $; startingAt: start ifAbsent: [ 0 ] ].
	values add: (ZnUtils trimString: (contentDisposition copyFrom: start to: contentDisposition size)).
	^ values
]

{ #category : #accessing }
ZnMimePart >> contentLength [
	self hasEntity ifTrue: [ ^ self entity contentLength ].
	(self hasHeaders and: [ self headers hasContentLength ]) ifTrue: [ ^ self headers contentLength ].
	^ nil
]

{ #category : #accessing }
ZnMimePart >> contentType [
	self hasEntity ifTrue: [ ^ self entity contentType ].
	(self hasHeaders and: [ self headers hasContentType ]) ifTrue: [ ^ self headers contentType ].
	^ nil
]

{ #category : #accessing }
ZnMimePart >> contents [
	^ entity isNil ifTrue: [ nil ] ifFalse: [ entity contents ]
]

{ #category : #private }
ZnMimePart >> detectContentDispositionValue: aString [
	self contentDispositionValues do: [ :each | 
		((each beginsWith: aString)
			and: [ each size > (aString size + 1)
			and: [ (each at: aString size + 1) = $= ] ]) ifTrue: [
				^ (each copyAfter: $=) withoutQuoting ] ].
	^ nil
]

{ #category : #accessing }
ZnMimePart >> entity [
	^ entity
]

{ #category : #accessing }
ZnMimePart >> entity: object [
	entity := object.
	self headers isDescribingEntity ifFalse: [
		self headers acceptEntityDescription: object ]
]

{ #category : #private }
ZnMimePart >> entityReaderOn: stream [
	^ ZnEntityReader new
		headers: self headers;
		stream: stream;
		allowReadingUpToEnd;
		yourself
]

{ #category : #private }
ZnMimePart >> entityWriterOn: stream [
	^ ZnEntityWriter new
		headers: self headers;
		stream: stream;
		yourself
]

{ #category : #accessing }
ZnMimePart >> fieldName [
	^ self detectContentDispositionValue: 'name'
]

{ #category : #accessing }
ZnMimePart >> fieldValue [
	^ self hasEntity 
		ifTrue: [ self entity contents ] 
		ifFalse: [ nil ] 
]

{ #category : #accessing }
ZnMimePart >> fieldValueString [
	^ self fieldValue
		ifNil: [ String empty ]
		ifNotNil: [ :value | value asString ]
]

{ #category : #accessing }
ZnMimePart >> fileName [
	"Pathnames are often silenty encoded using UTF-8,
	this is a no-op for ASCII, but will fail on Latin-1 and others"

	^ (self detectContentDispositionValue: 'filename')
		ifNotNil: [ :encodedFileName | encodedFileName asByteArray utf8Decoded ]
]

{ #category : #testing }
ZnMimePart >> hasEntity [
	^ self entity notNil
]

{ #category : #testing }
ZnMimePart >> hasHeaders [
	^ headers notNil and: [ self headers isEmpty not ]
]

{ #category : #comparing }
ZnMimePart >> hash [
	^ self headers hash bitXor: self entity hash
]

{ #category : #accessing }
ZnMimePart >> headers [
	headers isNil ifTrue: [ headers := ZnHeaders new ].
	^ headers
]

{ #category : #accessing }
ZnMimePart >> headers: object [
	headers := object
]

{ #category : #enumerating }
ZnMimePart >> headersDo: twoArgumentBlock [
	self hasHeaders
		ifTrue: [ self headers headersDo: twoArgumentBlock ]
]

{ #category : #copying }
ZnMimePart >> postCopy [
	headers := headers copy.
	"Note that we don't copy the entity, see also #resetEntity: and ZnClient>>#resetEntity"
]

{ #category : #printing }
ZnMimePart >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(.
	self hasEntity ifTrue: [ 
		self entity printContentTypeAndLengthOn: stream ].
	stream nextPut: $)
	
]

{ #category : #initialize-release }
ZnMimePart >> readBinaryFrom: stream [
	| entityReader |
	self headers: (ZnHeaders readFrom: stream).
	(entityReader := self entityReaderOn: stream)
		binary.
	self entity: entityReader readEntity
]

{ #category : #initialize-release }
ZnMimePart >> readFrom: stream [
	self headers: (ZnHeaders readFrom: stream).
	self entity: (self entityReaderOn: stream) readEntity
	
]

{ #category : #accessing }
ZnMimePart >> setContentDisposition: value [
	^ self headers at: 'Content-Disposition' put: value 
]

{ #category : #writing }
ZnMimePart >> writeOn: stream [
	self headers writeOn: stream.
	stream nextPutAll: String crlf.
	self hasEntity ifTrue: [ 
		(self entityWriterOn: stream) writeEntity: self entity ]
]
