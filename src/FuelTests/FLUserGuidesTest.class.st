"
SUnit tests to guide users with fuel serialization
"
Class {
	#name : #FLUserGuidesTest,
	#superclass : #FLSerializationTest,
	#category : #FuelTests
}

{ #category : #running }
FLUserGuidesTest >> tearDown [
	
	#('demo.fuel' 'numbers.fuel' 'number.fuel.zip' 'example.FL' ) 
		do: [:each | self deleteFileNamed: each ].
	super tearDown
		
]

{ #category : #'tests-Managing Globals' }
FLUserGuidesTest >> testConsiderGlobal [
	
	| aSerializer anArray materializedArray |
	"Prepare an array whose two elements are system globals."
	anArray := Array with: Set new with: Set new.
	Smalltalk at: #GlobalSet1 put: anArray first.
	Smalltalk at: #GlobalSet2 put: anArray second.

	"Serialize considering *only first* as a global object."
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aStream |
			aSerializer := FLSerializer newDefault.
			aSerializer analyzer considerGlobal: #GlobalSet1.
			aSerializer serialize: anArray on: aStream ].

	"Materialize"
	FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [:aStream |
			materializedArray := (FLMaterializer newDefault 
				materializeFrom: aStream) root].
				
	"Check that second element is a new Set."
	[ (Smalltalk at: #GlobalSet1) == materializedArray first ] assert.
	[ (Smalltalk at: #GlobalSet2) ~~ materializedArray second ] assert
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testDemo [

| sourceArray loadedArray |
sourceArray := 
	Array 
		with: 'a string' 
		with: Transcript
		with: [ Transcript show: 'a string'; flush ].
"Store to the file"
FLSerializer serialize: sourceArray toFileNamed: 'example.FL'. 
"Load from the file"
loadedArray := FLMaterializer materializeFromFileNamed: 'example.FL'. 
"The arrays are not the same"
[ sourceArray ~~ loadedArray ] assert.
"The strings are not the same"
[ sourceArray first ~~ loadedArray first ] assert.
[ sourceArray first = loadedArray first ] assert.
"The global instance Transcript is the same"
[ sourceArray second == loadedArray second ] assert.
"Look at Transcript how the loaded block prints a string"
loadedArray third value.
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testFileShortcuts [

| materializedString |
FLSerializer serialize: 'stringToSerialize' toFileNamed: 'demo.fuel'. 
materializedString := FLMaterializer materializeFromFileNamed: 'demo.fuel'.

]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testFileStream [

	| materializedString |
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aStream |
			FLSerializer newDefault 
				serialize: 'stringToSerialize' 
	 			on: aStream ].

	FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [ :aStream |
			materializedString := (FLMaterializer newDefault 
				materializeFrom: aStream ) root ]
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testGZip [

	| materialization |
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aFileStream | 
			|gzip|
			gzip := GZipWriteStream on: aFileStream.
			FLSerializer newDefault serialize: 123 on: gzip.
			gzip close ].
	
	FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [:aFileStream | 
			|gzip|
			gzip := GZipReadStream on: aFileStream.		
			materialization := FLMaterializer newDefault 
				materializeFrom: gzip ]
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testMemoryStream [

| anArray materializedString |
anArray := FLSerializer serializeToByteArray: 'stringToSerialize'. 
materializedString := FLMaterializer materializeFromByteArray: anArray.


]

{ #category : #'tests-Customizing Graph' }
FLUserGuidesTest >> testPluggableSubstitution [

	| objectToSerialize aSerializer |
	
	objectToSerialize := Array with: 'hello' with: '' writeStream.
	
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aStream |
			aSerializer := FLSerializer newDefault.
			aSerializer analyzer when: [ :o | o isStream ] substituteBy: [ :o | nil ].
			aSerializer		 
				serialize: objectToSerialize
	 			on: aStream ].

	FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [ :aStream |
			(FLMaterializer newDefault 
				materializeFrom: aStream) 
				root ]
]

{ #category : #'tests-Getting Started' }
FLUserGuidesTest >> testShowingProgressBar [

	(Smalltalk globals includesKey: #FLProgressSerialization) ifFalse: [
		" Needs:
		(ConfigurationOfFuel project latestVersion) load: 'FuelProgressUpdate'.
		"
		^self ].
	
	FLPlatform current
		fileNamed: 'numbers.fuel'
		writeStreamDo: [:aStream |
			FLSerializer newDefault 
				showProgress;
				serialize: (1 to: 200000) asArray 
				on: aStream ].
	
	FLPlatform current
		fileNamed: 'numbers.fuel'
		readStreamDo: [:aStream | 
			FLMaterializer newDefault 
				showProgress;
				materializeFrom: aStream ]
]
