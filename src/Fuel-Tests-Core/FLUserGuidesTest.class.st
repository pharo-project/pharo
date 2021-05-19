"
SUnit tests to guide users with fuel serialization
"
Class {
	#name : #FLUserGuidesTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLUserGuidesTest >> tearDown [
	#('demo.fuel' 'numbers.fuel' 'number.fuel.zip' 'example.FL')
		do: [ :each | self deleteFileNamed: each ].
	testingEnvironment removeKey: #GlobalSet1 ifAbsent: [  ].
	testingEnvironment removeKey: #GlobalSet2 ifAbsent: [  ].
	super tearDown
]

{ #category : #'tests-Managing Globals' }
FLUserGuidesTest >> testConsiderGlobal [
	
	| aSerializer anArray materializedArray |
	"Prepare an array whose two elements are system globals."
	anArray := Array with: Set new with: Set new.
	testingEnvironment at: #GlobalSet1 put: anArray first.
	testingEnvironment at: #GlobalSet2 put: anArray second.

	"Serialize considering *only first* as a global object."
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aStream |
			aSerializer := FLSerializer newDefault.
			aSerializer analyzer considerGlobal: #GlobalSet1.
			aSerializer serialize: anArray on: aStream ].

	"Materialize"
	materializedArray := FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [:aStream |
			(FLMaterializer newDefault 
				materializeFrom: aStream) root].
				
	"Check that second element is a new Set."
	[ (testingEnvironment at: #GlobalSet1) == materializedArray first ] assert.
	[ (testingEnvironment at: #GlobalSet2) ~~ materializedArray second ] assert
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testDemo [
	| sourceArray loadedArray |
	sourceArray := Array
		with: 'a string'
		with: Smalltalk
		with: [
			Smalltalk vm.
			'a string' ].
	"Store to the file"
	FLSerializer serialize: sourceArray toFileNamed: 'example.FL'.
	"Load from the file"
	loadedArray := FLMaterializer materializeFromFileNamed: 'example.FL'.
	"The arrays are not the same"
	self assert: sourceArray ~~ loadedArray.
	"The strings are not the same"
	self assert: sourceArray first ~~ loadedArray first.
	self assert: sourceArray first equals: loadedArray first.
	"The global instance Smalltalk is the same"
	self assert: sourceArray second identicalTo: loadedArray second.
	"Evaluating the loaded block prints a string"
	self assert: loadedArray third value equals: 'a string'
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testFileShortcuts [
	| materializedString |
	FLSerializer serialize: 'stringToSerialize' toFileNamed: 'demo.fuel'.
	materializedString := FLMaterializer
		materializeFromFileNamed: 'demo.fuel'.
	self assert: materializedString equals: 'stringToSerialize'
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testFileStream [
	| materializedString |
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo:
			[ :aStream | FLSerializer newDefault serialize: 'stringToSerialize' on: aStream ].
	materializedString := FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [ :aStream | 
			(FLMaterializer newDefault
				materializeFrom: aStream) root ].
	self assert: materializedString equals: 'stringToSerialize'
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testGZip [
	| materialized |
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aFileStream | 
			| gzip |
			gzip := GZipWriteStream on: aFileStream.
			FLSerializer newDefault serialize: 123 on: gzip.
			gzip finish ].
	materialized := FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [ :aFileStream | 
			| gzip |
			gzip := GZipReadStream on: aFileStream.
			(FLMaterializer newDefault materializeFrom: gzip) root ].
	self assert: materialized equals: 123
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testMemoryStream [
	| anArray materializedString |
	anArray := FLSerializer serializeToByteArray: 'stringToSerialize'.
	materializedString := FLMaterializer
		materializeFromByteArray: anArray.
	self assert: materializedString equals: 'stringToSerialize'
]

{ #category : #'tests-Customizing Graph' }
FLUserGuidesTest >> testPluggableSubstitution [
	| objectToSerialize aSerializer materializedObject |
	objectToSerialize := Array with: 'hello' with: '' writeStream.
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aStream | 
			aSerializer := FLSerializer newDefault.
			aSerializer analyzer
				when: [ :o | o isStream ]
				substituteBy: [ :o | nil ].
			aSerializer serialize: objectToSerialize on: aStream ].
	materializedObject := FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo:
			[ :aStream | (FLMaterializer newDefault materializeFrom: aStream) root ].
	self
		assert: materializedObject
		equals: (Array with: 'hello' with: nil)
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testShowingProgressBar [
	(testingEnvironment includesKey: #FLProgressSerialization)
		ifFalse: [ " Needs:
		(ConfigurationOfFuel project latestVersion) load: 'FuelProgressUpdate'.
		" ^ self skip ].
	FLPlatform current
		fileNamed: 'numbers.fuel'
		writeStreamDo: [ :aStream | 
			FLSerializer newDefault
				showProgress;
				serialize: (1 to: 200000) asArray on: aStream ].
	FLPlatform current
		fileNamed: 'numbers.fuel'
		readStreamDo: [ :aStream | 
			FLMaterializer newDefault
				showProgress;
				materializeFrom: aStream ]
]
