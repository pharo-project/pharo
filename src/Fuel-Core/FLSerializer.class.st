"
I am a binary object serializer.

An example of use:

| sourceArray loadedArray |
sourceArray := 
	Array 
		with: 'a string' 
		with: Transcript
		with: [ Transcript show: 'a string' ].

""Store to the file""
FLSerializer serialize: sourceArray toFileNamed: 'example.FL'. 

""Load from the file""
loadedArray := FLMaterializer materializeFromFileNamed: 'example.FL'. 

""The arrays are not the same""
[ sourceArray ~~ loadedArray ] assert.

""The strings are not the same""
[ sourceArray first ~~ loadedArray first ] assert.
[ sourceArray first = loadedArray first ] assert.

""The global instance Transcript is the same""
[ sourceArray second == loadedArray second ] assert.

""Appreciate in Transcript that the loaded block prints a string""
loadedArray third value.

"
Class {
	#name : #FLSerializer,
	#superclass : #Object,
	#instVars : [
		'analyzer',
		'serializationFactory',
		'signature',
		'version',
		'stream',
		'header'
	],
	#category : #'Fuel-Core-Base'
}

{ #category : #protected }
FLSerializer class >> currentVersion [
	^ 300
]

{ #category : #protected }
FLSerializer class >> defaultSignature [
	^ 'FUEL'
]

{ #category : #'instance creation' }
FLSerializer class >> newDefault [

	^self newLight
]

{ #category : #'instance creation' }
FLSerializer class >> newLight [

	^self new
]

{ #category : #'instance creation' }
FLSerializer class >> on: aStream [
	^ self newLight 
		stream: aStream;
		yourself
]

{ #category : #'serializing-shortcuts' }
FLSerializer class >> serialize: anObject on: aStream [

	self newDefault serialize: anObject on: aStream
]

{ #category : #'serializing-shortcuts' }
FLSerializer class >> serialize: anObject toFileNamed: aFilename [

	self newDefault serialize: anObject toFileNamed: aFilename
]

{ #category : #'serializing-shortcuts' }
FLSerializer class >> serializeToByteArray: anObject [
	"No stream is needed by the user. An internal in-memory stream will be used. This method returns a ByteArray representing the serialization"

	| aStream |
	aStream := WriteStream on: (ByteArray new: 100).
	self newDefault serialize: anObject on: aStream.
	^ aStream contents
]

{ #category : #header }
FLSerializer >> addPostMaterializationAction: aCleanBlockClosure [
	header addPostMaterializationAction: aCleanBlockClosure
]

{ #category : #header }
FLSerializer >> addPreMaterializationAction: aCleanBlockClosure [
	header addPreMaterializationAction: aCleanBlockClosure
]

{ #category : #accessing }
FLSerializer >> analyzer [
	^ analyzer ifNil: [ analyzer := self defaultAnalyzer ]
]

{ #category : #accessing }
FLSerializer >> analyzer: anObject [
	analyzer := anObject
]

{ #category : #header }
FLSerializer >> at: key putAdditionalObject: anObject [ 
	"This is useful if we want to attach objects to a package that will also be serialized. The way they are stored is key-value."
	header at: key putAdditionalObject: anObject 
]

{ #category : #protected }
FLSerializer >> defaultAnalyzer [

	^FLAnalyzer newDefault
]

{ #category : #protected }
FLSerializer >> encodeHeaderWith: anEncoder [

	"todo: fix how header is encoded"
	anEncoder encodeByte: (header isEmpty ifTrue: [0] ifFalse: [1]).

	header isEmpty ifFalse: [
		self class newDefault serializationFactory value: header value: anEncoder ]
]

{ #category : #protected }
FLSerializer >> encodeSignatureWith: anEncoder [
	anEncoder encodeBytes: self signature asByteArray
]

{ #category : #protected }
FLSerializer >> encodeVersionWith: anEncoder [
	anEncoder encodeUint16: self version

]

{ #category : #hooks }
FLSerializer >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #accessing }
FLSerializer >> header [
	^ header
]

{ #category : #initialization }
FLSerializer >> initialize [

	super initialize.
	self signature: self class defaultSignature.
	self version: self class currentVersion.
	header := FLHeader new.
]

{ #category : #protected }
FLSerializer >> serializationFactory [

	^ serializationFactory ifNil: [ 
		self setDefaultSerialization.
		serializationFactory ].
]

{ #category : #serializing }
FLSerializer >> serialize: anObject [
	"Serialize the graph starting at the root object received and answers the FLSerialization object"

	^ self serialize: anObject on: stream
]

{ #category : #serializing }
FLSerializer >> serialize: anObject on: aStream [
	"Serialize the graph starting at the root object received and answers the FLSerialization object"

	FLEncoder
		on: aStream 
		globalEnvironment: self analyzer globalEnvironment
		do: [ :anEncoder | 
			self encodeSignatureWith: anEncoder.
			self encodeVersionWith: anEncoder.
			self encodeHeaderWith: anEncoder.
			^ self serializationFactory value: anObject value: anEncoder ]
]

{ #category : #serializing }
FLSerializer >> serialize: anObject toFileNamed: aFilename [
	"Serialize the graph starting at the root object received and answers the FLSerialization object"
	FLPlatform current
		fileNamed: aFilename 
		writeStreamDo: [ :aFileStream |
			self serialize: anObject on: aFileStream ]
	
]

{ #category : #protected }
FLSerializer >> setDefaultSerialization [

	^ serializationFactory := [:anObject :anEncoder | 
		(FLSerialization 
			with: anEncoder
			root: anObject 
			analyzer: self analyzer)
			run;
			yourself ]
]

{ #category : #accessing }
FLSerializer >> signature [
	^ signature
]

{ #category : #accessing }
FLSerializer >> signature: anObject [
	signature := anObject
]

{ #category : #accessing }
FLSerializer >> stream: aStream [
	stream := aStream
]

{ #category : #accessing }
FLSerializer >> version [
	^ version
]

{ #category : #accessing }
FLSerializer >> version: anObject [
	version := anObject
]
