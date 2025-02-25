"
SUnit tests to guide users with fuel serialization
"
Class {
	#name : 'FLUserGuidesTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
FLUserGuidesTest >> tearDown [
	#('demo.fuel' 'numbers.fuel' 'number.fuel.zip' 'example.FL') do: [ :aFilename |
		self deleteFilePath: aFilename ].

	super tearDown
]

{ #category : 'tests-Getting Started' }
FLUserGuidesTest >> testByteArrayStream [
	| array materializedString |
	array := ByteArray streamContents: [ :stream |
		FLSerializer new
			streamFactory: [ stream ];
			object: 'stringToSerialize';
			serialize ].
	materializedString := FLMaterializer new
		streamFactory: [ array readStream ];
		materializeRoot.
	self assert: materializedString equals: 'stringToSerialize'
]

{ #category : 'tests-Managing Globals' }
FLUserGuidesTest >> testConsiderGlobal [
	
	| anArray materialized |
	"Prepare an array whose two elements are system globals."
	anArray := Array with: Set new with: Set new.
	self environmentOfTest
		at: #GlobalSet1 put: anArray first;
		at: #GlobalSet2 put: anArray second.
	addedGlobals
		add: #GlobalSet1;
		add: #GlobalSet2.

	"Serialize considering *only first* as a global object."
	FLSerializer new
		filePath: 'demo.fuel';
		environment: self environmentOfTest;
		lookUpInGlobals: #GlobalSet1;
		object: anArray;
		serialize.

	"Materialize"
	materialized := FLMaterializer new
		filePath: 'demo.fuel';
		environment: self environmentOfTest;
		materializeRoot.
				
	"Check that second element is a new Set."
	self assert: (self environmentOfTest at: #GlobalSet1) identicalTo: materialized first.
	self deny: (self environmentOfTest at: #GlobalSet2) identicalTo: materialized second
]

{ #category : 'tests-Getting Started' }
FLUserGuidesTest >> testDemo [
	| sourceArray loadedArray |
	Smalltalk compiler compilationContext optionCleanBlockClosure ifTrue: [ ^ self skip ].

	sourceArray := Array
		with: 'a string'
		with: Smalltalk
		with: [
			Smalltalk vm.
			'a string' ].
	"Store to the file"
	FLSerializer new
		filePath: 'example.FL';
		object: sourceArray;
		serialize.
	"Load from the file"
	loadedArray := FLMaterializer new
		filePath: 'example.FL';
		materializeRoot.
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

{ #category : 'tests-Getting Started' }
FLUserGuidesTest >> testFileShortcuts [
	| materializedString |
	FLSerializer new
		filePath: 'demo.fuel';
		object: 'stringToSerialize';
		serialize.
	materializedString := FLMaterializer new
		filePath: 'demo.fuel';
		materializeRoot.
	self assert: materializedString equals: 'stringToSerialize'
]

{ #category : 'tests-Getting Started' }
FLUserGuidesTest >> testGZip [
	| materialized |
	FLSerializer new
		filePath: 'demo.fuel';
		decorateStreamFactory: [ :fileStream | GZipWriteStream on: fileStream ];
		streamFinalizer: [ :gzipStream | gzipStream close ];
		object: 123;
		serialize.
	materialized := FLMaterializer new
		filePath: 'demo.fuel';
		decorateStreamFactory: [ :fileStream | GZipReadStream on: fileStream ];
		materializeRoot.
	self assert: materialized equals: 123
]

{ #category : 'tests-Customizing Graph' }
FLUserGuidesTest >> testPluggableSubstitution [
	| objectToSerialize materializedObject |
	objectToSerialize := Array with: 'hello' with: '' writeStream.
	FLSerializer new
		when: [ :o | o isStream ] substituteBy: [ :o | nil ];
		object: objectToSerialize;
		filePath: 'demo.fuel';
		serialize.
	materializedObject := FLMaterializer new
		filePath: 'demo.fuel';
		materializeRoot.
		
	self
		assert: materializedObject
		equals: (Array with: 'hello' with: nil)
]
