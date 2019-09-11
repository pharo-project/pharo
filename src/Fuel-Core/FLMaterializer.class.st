"
I am a binary object materializer. 

See an example of use in FLSerializer's documentation.


"
Class {
	#name : #FLMaterializer,
	#superclass : #Object,
	#instVars : [
		'materializationFactory',
		'signature',
		'version',
		'migrations',
		'globalEnvironment'
	],
	#category : #'Fuel-Core-Base'
}

{ #category : #protected }
FLMaterializer class >> currentVersion [
	^ 300
]

{ #category : #protected }
FLMaterializer class >> defaultSignature [
	^ 'FUEL'
]

{ #category : #'materializing-shortcuts' }
FLMaterializer class >> materializationFromFileNamed: aFilename [
	^ FLPlatform current
		fileNamed: aFilename 
		readStreamDo: [ :aFileStream | 
			self newDefault materializeFrom: aFileStream ]

]

{ #category : #'materializing-shortcuts' }
FLMaterializer class >> materializationHeaderFromFileNamed: aFilename [

	^ FLPlatform current
		fileNamed: aFilename 
		readStreamDo: [ :aFileStream | 
			self newDefault materializeHeaderFrom: aFileStream ]

]

{ #category : #'materializing-shortcuts' }
FLMaterializer class >> materializeFromByteArray: byteArray [

	^(self newDefault materializeFrom: byteArray readStream) root
]

{ #category : #'materializing-shortcuts' }
FLMaterializer class >> materializeFromFileNamed: aFilename [

	^ (self materializationFromFileNamed: aFilename) root

]

{ #category : #'materializing-shortcuts' }
FLMaterializer class >> materializeHeaderFromFileNamed: aFilename [

	^ self materializationHeaderFromFileNamed: aFilename

]

{ #category : #'instance creation' }
FLMaterializer class >> newDefault [

	^self new
]

{ #category : #protected }
FLMaterializer >> decodeHeaderWith: aDecoder [
	"See FLSerializer>>encodeHeaderWith:"
	
	"todo: fix"
	"Here I cannot use #materializeFrom: again because I will end up in an infinitive loop. Therefore, I use #materializationFactory."
	^(aDecoder nextEncodedByte = 0) 
		ifTrue: [ FLHeader new ]
		ifFalse: [ (self class newDefault materializationFactory value: aDecoder) root ]
]

{ #category : #hooks }
FLMaterializer >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #accessing }
FLMaterializer >> globalEnvironment [
	"Answer a dictionary where the look up for global symbols will be done during materialization."
	
	^ globalEnvironment
]

{ #category : #accessing }
FLMaterializer >> globalEnvironment: aDictionary [
	"Set the dictionary where the look up for global symbols will be done during materialization."
	
	globalEnvironment := aDictionary
]

{ #category : #initialization }
FLMaterializer >> initialize [

	super initialize.
	self setDefaultMaterialization.
	self signature: self class defaultSignature.
	self version: self class currentVersion.
	migrations := Dictionary new.
	globalEnvironment := self class environment.
]

{ #category : #accessing }
FLMaterializer >> materializationFactory [
	^ materializationFactory
]

{ #category : #materializing }
FLMaterializer >> materializeFrom: aStream [

	| aDecoder materialization header |
	aDecoder := 
		FLDecoder 
			on: aStream 
			migrations: migrations 
			globalEnvironment: globalEnvironment.
	self verifySignatureFrom: aDecoder.
	self verifyVersionFrom: aDecoder.
	header := self decodeHeaderWith: aDecoder.
	header executePreMaterializationActions.
	materialization := materializationFactory value: aDecoder.
	"This is useful because when the user materialize something, what it is answered is the materialization objet, and not the materializer. Hence, it is difficult to query the header (like asking the additionalObjects) because materializer is lost. Therefore, we also set the header to the materialization."
	materialization header: header. 
	"This is useful because the postMaterializationActions may need to have access to the state of the materialization, for example, to the root"
	header materialization: materialization.
	header executePostMaterializationActions.
	^ materialization 
	
]

{ #category : #materializing }
FLMaterializer >> materializeHeaderFrom: aStream [

	| aDecoder |
	aDecoder := 
		FLDecoder 
			on: aStream 
			migrations: migrations 
			globalEnvironment: globalEnvironment.
	self verifySignatureFrom: aDecoder.
	self verifyVersionFrom: aDecoder.
	^ self decodeHeaderWith: aDecoder
]

{ #category : #'configuring-migration' }
FLMaterializer >> migrateClassNamed: aSymbol toClass: aClass [

	self 
		migrateClassNamed: aSymbol 
		toClass: aClass 
		variables: #()
]

{ #category : #'configuring-migration' }
FLMaterializer >> migrateClassNamed: aSymbol toClass: aClass variables: aDictionary [ 

	migrations
		at: aSymbol
		put: (FLMigration fromClassNamed: aSymbol toClass: aClass variables: aDictionary)
]

{ #category : #'configuring-migration' }
FLMaterializer >> migrateClassNamed: aSymbol variables: aDictionary [ 

	self 
		migrateClassNamed: aSymbol 
		toClass: (self class environment at: aSymbol)
		variables: aDictionary
]

{ #category : #protected }
FLMaterializer >> setDefaultMaterialization [

	materializationFactory := [:aDecoder | 
		(FLMaterialization 
			with: aDecoder)
			run; 
			yourself ]
	
	

]

{ #category : #accessing }
FLMaterializer >> signature [
	^ signature
]

{ #category : #accessing }
FLMaterializer >> signature: anObject [
	signature := anObject
]

{ #category : #protected }
FLMaterializer >> verifySignatureFrom: aDecoder [

	| streamSignature |
	streamSignature := ByteArray new: self signature size.
	aDecoder nextEncodedBytesInto: streamSignature.
	(self signature asByteArray = streamSignature) ifFalse: [ 
		FLBadSignature 
			signalCurrentSignature: self signature 
			streamSignature: streamSignature  ].

]

{ #category : #protected }
FLMaterializer >> verifyVersionFrom: aDecoder [

	| streamVersion |
	streamVersion := aDecoder nextEncodedUint16.
	(self version = streamVersion) ifFalse: [ 
		FLBadVersion 
			signalCurrentVersion: self version 
			streamVersion: streamVersion ]

]

{ #category : #accessing }
FLMaterializer >> version [
	^ version
]

{ #category : #accessing }
FLMaterializer >> version: anObject [
	version := anObject
]
