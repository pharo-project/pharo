"
Hudson report for test results
"
Class {
	#name : #HDTestReport,
	#superclass : #HDReport,
	#instVars : [
		'suite',
		'stream',
		'suitePosition',
		'suiteTime',
		'suiteFailures',
		'suiteErrors',
		'progressFile',
		'nodeName',
		'stageName'
	],
	#classVars : [
		'CurrentStageName'
	],
	#category : #'JenkinsTools-Core'
}

{ #category : #running }
HDTestReport class >> currentStageName: aStageName [
	CurrentStageName := aStageName
]

{ #category : #running }
HDTestReport class >> runClasses: aCollectionOfClasses named: aString [
	| suite classes |
	suite := TestSuite named: aString.
	classes := (aCollectionOfClasses
		select: [ :each | (each includesBehavior: TestCase) and: [ each isAbstract not ] ])
			asSortedCollection: [ :a :b | a name <= b name ].
	classes isEmpty
		ifTrue: [ ^ nil ].
	classes
		do: [ :each | each addToSuiteFromSelectors: suite ].
	^ self runSuite: suite
]

{ #category : #running }
HDTestReport class >> runPackage: aString [
	^ self runClasses: (RPackage organizer packageNamed: aString) definedClasses named: aString
]

{ #category : #running }
HDTestReport class >> runSuite: aTestSuite [
	^ self new
		initializeOn: aTestSuite;
		stageName: CurrentStageName;
		run;
		done
]

{ #category : #private }
HDTestReport >> beginTestCase: aTestCase runBlock: aBlock [
	| time internalLog |
	progressFile
		nextPutAll: 'starting testcase: ';
		nextPutAll: aTestCase class name;
		nextPutAll: '>>';
		nextPutAll: aTestCase selector;
		nextPutAll: ' ... ';
		flush.
	"The test element must be written after test completion to include a time information about run.
	Therefore we can't report any error during test directly into the main xml stream.
	Otherwise any new xml element would be written out of the test node.
	To allow reporting during test we set a temp log stream 
	which we append to the main stream at the end of test as part of test element"
	internalLog := self newLogDuring: [ time := aBlock timeToRun ].
	stream
		tab;
		nextPutAll: '<testcase classname="';
		nextPutAll:
			(self encode: (self generateTestName: aTestCase));
		nextPut: $.;
		nextPutAll: (self encode: aTestCase class name);
		nextPutAll: '" name="';
		nextPutAll: (self encode: aTestCase selector);
		nextPutAll: '" time="';
		print: time asMilliSeconds / 1000.0;
		nextPutAll: '">';
		lf.
	stream nextPutAll: internalLog
]

{ #category : #private }
HDTestReport >> calculateNodeName [
	| environmentClass name bitString |
	bitString := Smalltalk vm is32bit
		ifTrue: [ '32' ]
		ifFalse: [ '64' ].

	name := Smalltalk os family , bitString , '.'.

	" If the environment class is not available, because it needs FFI, I use the node name,
	because for sure I am in the Bootstrap process."
	environmentClass := Smalltalk at: #OSEnvironment ifAbsent: [ ^ name ].

	^ environmentClass current
		at: 'JENKINS_HOME'
		ifPresent: [ :value | name ]
		ifAbsent: [ '' ]
]

{ #category : #running }
HDTestReport >> done [
	"just close the file"
	[ progressFile close ] on: Error do: []
]

{ #category : #private }
HDTestReport >> endTestCase [
	stream tab; nextPutAll: '</testcase>'; lf.
	progressFile nextPutAll: 'finished' ; crlf; flush.

]

{ #category : #private }
HDTestReport >> fuelFileNameForNextErrorDuring: aTestCase [
		
	| fileName errorsCount testName |
	testName := aTestCase class name asString, '-', aTestCase selector.
	fileName := testName, '.fuel'.
	errorsCount := 1.
	[ File exists: fileName ] whileTrue: [ 
		errorsCount := errorsCount + 1.
		fileName := testName , '_' , errorsCount asString , '.fuel' ].
	^fileName 
]

{ #category : #private }
HDTestReport >> generateTestName: aTestCase [
	^ nodeName , (aTestCase class category copyReplaceAll: '-' with: '.')
]

{ #category : #testing }
HDTestReport >> hasErrors [ 
	^ suiteErrors ~= 0
]

{ #category : #testing }
HDTestReport >> hasFailures [ 
	^ suiteFailures ~= 0
]

{ #category : #testing }
HDTestReport >> hasFailuresOrErrors [ 
	^ self hasFailures or: [ self hasErrors ]
]

{ #category : #initialization }
HDTestReport >> initialize [

	| aFile |

	super initialize.
	suiteTime := 0 asDuration.
	suitePosition := suiteFailures := suiteErrors := 0.
	
	aFile := File named: 'progress.log'.
	aFile delete.
	progressFile := ZnCharacterWriteStream
			on: (aFile writeStream setToEnd; yourself)
			encoding: 'utf8'.
			
	nodeName := self calculateNodeName.
	stageName := ''
]

{ #category : #initialization }
HDTestReport >> initializeOn: aTestSuite [
	suite := aTestSuite
]

{ #category : #private }
HDTestReport >> newLogDuring: aBlock [

	| currentStream tempStream |
	currentStream := stream.
	stream := tempStream := String new writeStream.
	aBlock ensure: [ stream := currentStream ].
	^tempStream contents
	 
]

{ #category : #running }
HDTestReport >> recordError: anError duringTest: aTestCase [ 

	| stack |
	aTestCase shouldPass ifTrue: [ self serializeError: anError of: aTestCase ].
		
	stack := self stackTraceString: anError of: aTestCase.
	self writeError: anError andStack: stack for: aTestCase
]

{ #category : #running }
HDTestReport >> recordUnexpectedPass [

	self writeFailure: TestFailure new stack: 'Unexpected failure' 
]

{ #category : #running }
HDTestReport >> run [
	Author uniqueInstance
		ifUnknownAuthorUse: 'hudson'
		during: [ [ 
			self setUp.
			suiteTime := [ self runAll ]
				timeToRun ]
					ensure: [ self tearDown ] ]
]

{ #category : #running }
HDTestReport >> runAll [
	CurrentExecutionEnvironment runTestsBy: [ 
		suite tests do: [ :each | each run: self ]
	]
]

{ #category : #running }
HDTestReport >> runCase: aTestCase [
	self
		beginTestCase: aTestCase
		runBlock: [ 
			[aTestCase runCaseManaged.
			aTestCase shouldPass ifFalse: [ self recordUnexpectedPass ]] 
				on: Exception - UnhandledError 
				do: [ :exc | exc recordResultOf: aTestCase inHDTestReport: self ]
		].			
	self endTestCase
]

{ #category : #running }
HDTestReport >> serializeError: error of: aTestCase [
	"We got an error from a test, let's serialize it so we can properly debug it later on..."
	
	self class environment at: #FuelOutStackDebugAction ifPresent: [ :fuelOutStackDebugAction |
		| context testCaseMethodContext fileName |
		context := error signalerContext.
		testCaseMethodContext := context findContextSuchThat: [ :ctx| 
			ctx receiver == aTestCase and: [ ctx methodSelector == #performTest ]].
		context := context copyTo: testCaseMethodContext.
		
		[fileName := self fuelFileNameForNextErrorDuring: aTestCase.
		fuelOutStackDebugAction serializeTestFailureContext: context sender toFileNamed: fileName	] 
			on: Error do: [:err| "simply continue..." ]
	].


]

{ #category : #running }
HDTestReport >> setUp [

	| aFile fileName |
	
	progressFile nextPutAll: 'running suite: ';
		nextPutAll: suite name ; crlf; flush.

	fileName := stageName isEmpty ifTrue: [ '' ] ifFalse: [ stageName , '-' ].
	fileName := fileName , suite name , '-Test.xml'.
		
	aFile := File named: fileName .
	aFile delete.
	stream := ZnCharacterWriteStream
			on: (aFile writeStream setToEnd; yourself)
			encoding: 'utf8'.	
		
	stream nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>'; lf.
	stream nextPutAll: '<testsuite name="'; nextPutAll: (self encode: suite name); 
		nextPutAll: '" tests="'; print: suite tests size; nextPutAll: '">'.
	
	"Now this is ugly. We want to update the time and the number of failures and errors, but still at the same time stream a valid XML. So remember this position and add some whitespace, that we can fill later."
	suitePosition := stream wrappedStream position - 1.
	stream nextPutAll: (String new: 100 withAll: $ ); lf.
	
	"Initialize the test resources."
	suite resources do: [ :each |
		each isAvailable
			ifFalse: [ each signalInitializationError ] ]
]

{ #category : #private }
HDTestReport >> stackTraceString: err of: aTestCase [
	^ String streamContents: [ :str | 
		| context |
		context := err signalerContext.
		[ context isNil or: [ context receiver == aTestCase and: [ context methodSelector == #runCase ] ] ] whileFalse: [
			[str print: context; lf.] onErrorDo: [ str nextPutAll: 'PRINTING ERROR'; lf].
			context := context sender ] ] 
]

{ #category : #accessing }
HDTestReport >> stageName [
	"The stage name is used by the CI to name the report files"
	^ stageName
]

{ #category : #accessing }
HDTestReport >> stageName: anObject [
	stageName := anObject
]

{ #category : #accessing }
HDTestReport >> suiteErrors [
	^ suiteErrors
]

{ #category : #accessing }
HDTestReport >> suiteFailures [
	^ suiteFailures
]

{ #category : #accessing }
HDTestReport >> suitePassing [ 
	^ self suiteTotal - self suiteFailures - self suiteErrors
]

{ #category : #accessing }
HDTestReport >> suiteTotal [
	^ suite 
		ifNotNil: [ suite tests size ]
		ifNil: [ 0 ]
]

{ #category : #running }
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
		nextPutAll: '" time="'; print: suiteTime asMilliSeconds / 1000.0; 
		nextPutAll: '">'.
	stream close.
	
	progressFile 
		nextPutAll: 'finished running suite: ';
		nextPutAll: suite name;
		close
]

{ #category : #private }
HDTestReport >> writeError: anError andStack: aString for: aTestCase [
	"XML tag is now open"
	anError
		ifNotNil: [
			aTestCase shouldPass ifTrue: [
				(anError isKindOf: TestFailure)
					ifTrue: [ self writeFailure: anError stack: aString ]
					ifFalse: [ self writeError: anError stack: aString ] ] ]
		ifNil: [
			aTestCase shouldPass ifFalse: [
				self writeFailure: TestFailure new stack: 'Unexpected failure' ] ]
]

{ #category : #private }
HDTestReport >> writeError: error stack: stack [
	suiteErrors := suiteErrors + 1.
	stream tab; tab; nextPutAll: '<error type="'.
	self writeException: error stack: stack.
	stream nextPutAll: '</error>'; lf
]

{ #category : #private }
HDTestReport >> writeException: error stack: stack [
	| encodedErrorName encodedErrorDescription |
	encodedErrorName := self encode: error class name.
	encodedErrorDescription := self
		encode: (error messageText ifNil: [ error description ]).
	stream
		nextPutAll: encodedErrorName;
		nextPutAll: '" message="';
		nextPutAll: encodedErrorDescription;
		nextPutAll: '">';
		lf;
		nextPutAll: encodedErrorName;
		lf.
	encodedErrorDescription ifNotEmpty: [ 
		stream nextPutAll: encodedErrorDescription;
		lf ].
	stream nextPutAll: (self encode: stack)
]

{ #category : #private }
HDTestReport >> writeFailure: error stack: stack [
	suiteFailures := suiteFailures + 1.
	stream tab; tab; nextPutAll: '<failure type="'.
	self writeException: error stack: stack.  
	stream	nextPutAll: '</failure>'; lf
]
