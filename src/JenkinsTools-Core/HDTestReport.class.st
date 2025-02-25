"
Hudson report for test results
"
Class {
	#name : 'HDTestReport',
	#superclass : 'HDReport',
	#instVars : [
		'suite',
		'stream',
		'suitePosition',
		'suiteTime',
		'suiteFailures',
		'suiteErrors',
		'nodeName',
		'stageName',
		'progressFileName',
		'progressStream',
		'shouldSerializeError'
	],
	#classVars : [
		'CurrentStageName',
		'ShuffleSeed'
	],
	#category : 'JenkinsTools-Core',
	#package : 'JenkinsTools-Core'
}

{ #category : 'running' }
HDTestReport class >> currentStageName [
	^CurrentStageName ifNil: [ '' ]
]

{ #category : 'running' }
HDTestReport class >> currentStageName: aStageName [
	CurrentStageName := aStageName
]

{ #category : 'running' }
HDTestReport class >> runClasses: aCollectionOfClasses named: packageName [

	| suite classes time result |
	suite := TestSuite named: packageName.
	ShuffleSeed ifNotNil: [ suite shuffleSeed: ShuffleSeed asInteger ].
	classes := (aCollectionOfClasses select: [ :class |
		            class isTestCase and: [ class isAbstract not ] ])
		           asSortedCollection: [ :a :b | a name <= b name ].
	classes ifEmpty: [ ^ nil ].
	classes do: [ :class | suite addTests: class buildSuite tests ].
	time := DateAndTime now.
	('Beginning to run tests of ' , packageName , ' with random seed '
	 , suite shuffleSeed asString , OSPlatform current lineEnding)
		traceCr.
	result := self runSuite: suite.
	('Finished to run tests of ' , packageName , ' in '
	 , (DateAndTime now - time) humanReadablePrintString
	 , OSPlatform current lineEnding) traceCr.
	^ result 
]

{ #category : 'running' }
HDTestReport class >> runPackage: aString [

	^ self runClasses: (self packageOrganizer packageNamed: aString) definedClasses named: aString
]

{ #category : 'running' }
HDTestReport class >> runSuite: aTestSuite [

	^ self new runSuite: aTestSuite
]

{ #category : 'running' }
HDTestReport class >> shuffleSeed: aSeed [
	ShuffleSeed := aSeed
]

{ #category : 'private' }
HDTestReport >> calculateNodeName [
	| environmentClass name bitString |
	bitString := Smalltalk vm is32bit
		ifTrue: [ '32' ]
		ifFalse: [ '64' ].

	name := Smalltalk os family , bitString , '.'.

	"The Stage name has kernel in the name when running the tests in the small image.
	We need to keep record of that"
	(self stageName includesSubstring: 'Kernel')
		ifTrue: [name := name , '.Kernel' ].

	" If the environment class is not available, because it needs FFI, I use the node name,
	because for sure I am in the Bootstrap process."
	environmentClass := Smalltalk at: #OSEnvironment ifAbsent: [ ^ name ].

	^ environmentClass current
		at: 'JENKINS_HOME'
		ifPresent: [ :value | name ]
		ifAbsent: [ '' ]
]

{ #category : 'running' }
HDTestReport >> done [
	"just close the file"
	[ progressStream close ] on: Error do: []
]

{ #category : 'private' }
HDTestReport >> generateTestName: aTestCase [

	^ nodeName , (aTestCase class package name copyReplaceAll: '-' with: '.')
]

{ #category : 'testing' }
HDTestReport >> hasErrors [ 
	^ suiteErrors ~= 0
]

{ #category : 'testing' }
HDTestReport >> hasFailures [ 
	^ suiteFailures ~= 0
]

{ #category : 'testing' }
HDTestReport >> hasFailuresOrErrors [ 
	^ self hasFailures or: [ self hasErrors ]
]

{ #category : 'initialization' }
HDTestReport >> initialize [

	super initialize.
	shouldSerializeError := true.
	suiteTime := 0.
	suitePosition := suiteFailures := suiteErrors := 0.
	progressFileName := 'progress.log'.
	stageName := self class currentStageName.
	nodeName := self calculateNodeName.
	
]

{ #category : 'initialization' }
HDTestReport >> initializeOn: aTestSuite [
	suite := aTestSuite
]

{ #category : 'private' }
HDTestReport >> newLogDuring: aBlock [

	| currentStream tempStream |
	currentStream := stream.
	stream := tempStream := String new writeStream.
	aBlock ensure: [ stream := currentStream ].
	^tempStream contents
]

{ #category : 'initialization' }
HDTestReport >> openProgressStream [ 
	
	| aFile |
	aFile := File named: progressFileName.
	aFile delete.
	progressStream := ZnCharacterWriteStream
			on: (aFile writeStream setToEnd; yourself)
			encoding: 'utf8'
]

{ #category : 'accessing' }
HDTestReport >> progressFileName [
	^ progressFileName
]

{ #category : 'accessing' }
HDTestReport >> progressFileName: anObject [
	progressFileName := anObject
]

{ #category : 'running' }
HDTestReport >> recordError: anError duringTest: aTestCase [ 

	aTestCase shouldPass ifFalse: [ ^self ].
	suiteErrors := suiteErrors + 1.
				
	self writeError: anError of: aTestCase.
	self serializeError: anError of: aTestCase.
]

{ #category : 'running' }
HDTestReport >> recordFailure: aTestFailure duringTest: aTestCase [ 

	aTestCase shouldPass ifFalse: [ ^self ].
	suiteFailures := suiteFailures + 1.	
			
	self writeFailure: aTestFailure of: aTestCase.
	self serializeError: aTestFailure of: aTestCase.
]

{ #category : 'running' }
HDTestReport >> recordPassOf: aTestCase [
]

{ #category : 'running' }
HDTestReport >> recordSkip: aTestSkip duringTest: aTestCase [
]

{ #category : 'running' }
HDTestReport >> recordUnexpectedPassOf: aTestCase [
	suiteFailures := suiteFailures + 1.
	
	self 
		writeException: (TestFailure new messageText: 'Unexpected pass (test is marked as expected failure)') 
		asNode: 'failure'
		using: [  ]
]

{ #category : 'private' }
HDTestReport >> reportTestCase: aTestCase runBlock: aBlock [
	| time testLog |
	progressStream
		nextPutAll: 'starting testcase: ';
		nextPutAll: aTestCase class name;
		nextPutAll: '>>';
		nextPutAll: aTestCase nameForReport;
		nextPutAll: ' ... ';
		flush.
	"The test element must be written after test completion to include a time information about run.
	Therefore we can't report any error during test directly into the main xml stream.
	Otherwise any new xml element would be written out of the test node.
	To allow reporting during test we set a temp log stream 
	which we append to the main stream at the end of test as part of test element"
	testLog := self newLogDuring: [ time := aBlock millisecondsToRun ].
	stream
		tab; nextPutAll: '<testcase classname="';
		nextPutAll: (self encode: (self generateTestName: aTestCase));
		nextPut: $.;
		nextPutAll: (self encode: aTestCase class name);
		nextPutAll: '" name="';
		nextPutAll: (self encode: aTestCase nameForReport);
		nextPutAll: '" time="';
		print: time / 1000.0;
		nextPutAll: '">';
		lf.
	stream nextPutAll: testLog.
	stream tab; nextPutAll: '</testcase>'; lf.
	progressStream
		nextPutAll: 'finished in ';
		nextPutAll: (Duration milliSeconds: time) humanReadablePrintString;
		crlf;
		flush
]

{ #category : 'running' }
HDTestReport >> run [

	[
	self setUp.
	suiteTime := [ self runAll ] millisecondsToRun ] ensure: [
		self tearDown ]
]

{ #category : 'running' }
HDTestReport >> runAll [
	CurrentExecutionEnvironment runTestsBy: [ 
		suite tests do: [ :each | each run: self ]
	]
]

{ #category : 'running' }
HDTestReport >> runCase: aTestCase [
	self
		reportTestCase: aTestCase
		runBlock: [ 
			[aTestCase runCaseManaged.
			aTestCase shouldPass 
				ifTrue: [ self recordPassOf: aTestCase ]
				ifFalse: [ self recordUnexpectedPassOf: aTestCase ]
			] on: Exception do: [ :exc | 
					exc recordResultOf: aTestCase inHDTestReport: self ]
		]
]

{ #category : 'running' }
HDTestReport >> runSuite: aTestSuite [
	^ self
		initializeOn: aTestSuite;
		run;
		done
]

{ #category : 'running' }
HDTestReport >> serializeError: error of: aTestCase [

	"We got an error from a test, let's serialize it so we can properly debug it later on..."

	| fuelFileName |
	
	self shouldSerializeError ifFalse: [ ^ self ].
	
	self class environment
		at: #FLDebuggerStackSerializer
		ifPresent: [ :fuelOutStackDebugAction | 
			| context testCaseMethodContext |
			"we use signalContext and findMethodContextSuchThat: to find the method even with clean blocks in the middle"
 			context := error signalContext.
			testCaseMethodContext := context findMethodContextSuchThat: [ :ctx | 
				                         ctx receiver == aTestCase and: [ 
					                         ctx selector == #performTest ] ].
			context := context copyTo: testCaseMethodContext.

			fuelFileName := self suiteFileNameWithoutExtension , ('-' , aTestCase class name asString , '-',  aTestCase selector asString , '.fuel').

			[ fuelOutStackDebugAction 
					serializeStackFromContext: context sender 
					toFileNamed: fuelFileName ]
				on: Error
				do: [ :err | Stdio stderr << err messageText; crlf; flush ] ]
]

{ #category : 'running' }
HDTestReport >> setUp [

	| aFile |
	self openProgressStream.
	progressStream nextPutAll: 'running suite: ';
		nextPutAll: suite name ; crlf; flush.

	aFile := File named: self suiteFileNameWithoutExtension , '.xml' .
	aFile delete.
	stream := ZnCharacterWriteStream
			on: (aFile writeStream setToEnd; yourself)
			encoding: 'utf8'.	
		
	stream nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>'; lf.
	stream
		nextPutAll: '<testsuite ';
		nextPutAll: 'name="'; nextPutAll: (self encode: suite name); nextPutAll: '" ';
		nextPutAll: 'tests="'; print: suite tests size; nextPutAll: '" ';
		nextPutAll: 'timestamp="'; print: Time now; nextPutAll: '" ';
		nextPutAll: 'seed="'; print: suite shuffleSeed; nextPutAll: '" ';
		nextPutAll: '>'.
	
	"Now this is ugly. We want to update the time and the number of failures and errors, but still at the same time stream a valid XML. So remember this position and add some whitespace, that we can fill later."
	suitePosition := stream wrappedStream position - 1.
	stream nextPutAll: (String new: 100 withAll: $ ); lf.
	
	"Initialize the test resources."
	suite resources do: [ :each |
		each isAvailable
			ifFalse: [ each signalInitializationError ] ]
]

{ #category : 'accessing' }
HDTestReport >> shouldSerializeError [
	^ shouldSerializeError
]

{ #category : 'accessing' }
HDTestReport >> shouldSerializeError: aBoolean [
	shouldSerializeError := aBoolean.
]

{ #category : 'private' }
HDTestReport >> stackTraceString: err of: aTestCase [
	^ self newLogDuring: [ self writeExceptionStack: err of: aTestCase ]
]

{ #category : 'accessing' }
HDTestReport >> stageName [
	"The stage name is used by the CI to name the report files"
	^ stageName
]

{ #category : 'accessing' }
HDTestReport >> stageName: anObject [
	stageName := anObject
]

{ #category : 'accessing' }
HDTestReport >> suiteErrors [
	^ suiteErrors
]

{ #category : 'accessing' }
HDTestReport >> suiteFailures [
	^ suiteFailures
]

{ #category : 'accessing' }
HDTestReport >> suiteFileNameWithoutExtension [

	| fileName |
	fileName := stageName isEmpty
		            ifTrue: [ '' ]
		            ifFalse: [ stageName , '-' ].
	^ fileName , suite name , '-Test'
]

{ #category : 'accessing' }
HDTestReport >> suitePassing [ 
	^ self suiteTotal - self suiteFailures - self suiteErrors
]

{ #category : 'accessing' }
HDTestReport >> suiteTotal [
	^ suite 
		ifNotNil: [ suite tests size ]
		ifNil: [ 0 ]
]

{ #category : 'running' }
HDTestReport >> tearDown [
	suite resources 
		do: [ :each | each reset ].
		
	stream tab; nextPutAll: '<system-out><![CDATA[]]></system-out>'; lf.
	stream tab; nextPutAll: '<system-err><![CDATA[]]></system-err>'; lf.
	stream nextPutAll: '</testsuite>'.
	
	stream wrappedStream position: suitePosition.
	stream 
		nextPutAll: ' failures="'; print: suiteFailures; 
		nextPutAll: '" errors="'; print: suiteErrors; 
		nextPutAll: '" time="'; print: suiteTime / 1000.0; 
		nextPutAll: '">'.
	stream close.
	
	progressStream 
		nextPutAll: 'finished running suite: ';
		nextPutAll: suite name;
		close
]

{ #category : 'private' }
HDTestReport >> writeError: anError of: aTestCase [

	self writeException: anError of: aTestCase asNode: 'error'
]

{ #category : 'private' }
HDTestReport >> writeException: anException asNode: errorNodeName using: writeBlock [
	| encodedErrorName encodedErrorDescription |
	stream tab; tab; nextPutAll: '<' ; nextPutAll: errorNodeName; nextPutAll: ' type="'.
	encodedErrorName := self encode: anException class name.
	encodedErrorDescription := self
		encode: (anException messageText ifNil: [ anException description ]).
	stream
		nextPutAll: encodedErrorName;
		nextPutAll: '" message="'; 	nextPutAll: encodedErrorDescription; nextPutAll: '">';
		lf; nextPutAll: encodedErrorName;	lf.
	encodedErrorDescription ifNotEmpty: [ 
		stream nextPutAll: encodedErrorDescription;
		lf ].
	writeBlock value.
	stream tab; tab; nextPutAll: '</'; nextPutAll: errorNodeName; nextPutAll: '>'; lf
]

{ #category : 'private' }
HDTestReport >> writeException: anException of: aTestCase asNode: errorNodeName [

	self  
		writeException: anException 
		asNode: errorNodeName 
		using: [ 	self writeExceptionStack: anException of: aTestCase ]
]

{ #category : 'private' }
HDTestReport >> writeExceptionStack: anException of: aTestCase [
	| context |
	context := anException signalerContext.
	[ context isNil or: [ context receiver == aTestCase and: [ context selector == #runCase ]]]
		whileFalse: [
			[ stream nextPutAll: (self encode: context printString); lf ] 
				onErrorDo: [ stream nextPutAll: 'PRINTING ERROR'; lf].
			context := context sender ]
]

{ #category : 'private' }
HDTestReport >> writeFailure: aTestFailure of: aTestCase [

	self writeException: aTestFailure of: aTestCase asNode: 'failure'
]
