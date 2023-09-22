"
Report tests execution result under XML format
"
Class {
	#name : 'XmlReporter',
	#superclass : 'AbstractReporter',
	#instVars : [
		'stream',
		'suitePosition'
	],
	#category : 'TestRunner-Core-Reporters',
	#package : 'TestRunner-Core',
	#tag : 'Reporters'
}

{ #category : 'reporting' }
XmlReporter class >> buildAttributesForFailure: aTestFailure duringTest: aTestCase [

	| aDictionnary |
	aDictionnary := Dictionary new.
	aDictionnary
		at: 'type' put: aTestFailure class;
		at: 'message' put: aTestFailure messageText.

	^ aDictionnary
]

{ #category : 'reporting' }
XmlReporter class >> convert: aString [

	^ (aString asString
		   copyReplaceAll: (String with: Character cr with: Character lf)
		   with: (String with: Character lf))
		  copyReplaceAll: (String with: Character cr)
		  with: (String with: Character lf)
]

{ #category : 'reporting' }
XmlReporter class >> encode: aString [

	^ ((aString asString copyReplaceAll: '&' with: '&amp;')
		   copyReplaceAll: '"'
		   with: '&quot;') copyReplaceAll: '<' with: '&lt;'
]

{ #category : 'reporting' }
XmlReporter class >> generateTesCaseClassName: aTestCase [

	^ (aTestCase class category copyReplaceAll: '-' with: '.') , '.'
	  , aTestCase class name
]

{ #category : 'private' }
XmlReporter >> buildAttributesForFailure: aTestFailure duringTest: aTestCase [

	^ self class
		  buildAttributesForFailure: aTestFailure
		  duringTest: aTestCase
]

{ #category : 'reporting' }
XmlReporter >> buildAttributesForTestCase: aTestCase [

	| aDictionnary |
	aDictionnary := Dictionary new.
	aDictionnary
		at: 'classname' put: (self generateTesCaseClassName: aTestCase);
		at: 'name' put: aTestCase nameForReport;
		at: 'time' put: self currentCaseDuration.

	^ aDictionnary
]

{ #category : 'reporting' }
XmlReporter >> convert: aString [

	^ self class convert: aString
]

{ #category : 'reporting' }
XmlReporter >> currentCaseDuration [

	^ currentCaseDuration
]

{ #category : 'reporting' }
XmlReporter >> datasAreReseted [

	^ super datasAreReseted and: (stream isNil and: suitePosition isNil)
]

{ #category : 'reporting' }
XmlReporter >> encode: aString [

	^ self class encode: aString
]

{ #category : 'reporting' }
XmlReporter >> finishReporting [

	stream close
]

{ #category : 'reporting' }
XmlReporter >> generateStandardPart [

	stream
		nextPutAll: self tabCaracter;
		nextPutAll: '<system-out><![CDATA[]]></system-out>';
		lf;
		nextPutAll: self tabCaracter;
		nextPutAll: '<system-err><![CDATA[]]></system-err>';
		lf
]

{ #category : 'reporting' }
XmlReporter >> generateTesCaseClassName: aTestCase [

	^ self class generateTesCaseClassName: aTestCase
]

{ #category : 'reporting' }
XmlReporter >> reportError: aTestError of: aTestCase [

	self reportFailure: aTestError of: aTestCase
]

{ #category : 'reporting' }
XmlReporter >> reportFailure: aTestFailure of: aTestCase [

	| attributesForFailureTag attributesForTestCaseTag |
	attributesForFailureTag := self
		                           buildAttributesForFailure: aTestFailure
		                           duringTest: aTestCase.
	attributesForTestCaseTag := self buildAttributesForTestCase:
		                            aTestCase.
	[ 
	self
		writeOpeningTestCaseTag: aTestCase
		withProperties: attributesForTestCaseTag;
		writeOpeningFailureTag: aTestFailure
		withProperties: attributesForFailureTag.
	stream nextPutAll: self tabCaracter3Times.
	self writeException: aTestFailure of: aTestCase ] ensure: [ 
		stream
			nextPutAll: self tabCaracter2Times;
			nextPutAll: '</failure>';
			lf.
		self writeClosingTag: aTestCase ]
]

{ #category : 'reporting' }
XmlReporter >> reportPass: aTestCase [

	"report the execution's result of a passed test"

	self
		writeOpeningTestCaseTag: aTestCase
		withProperties: (self buildAttributesForTestCase: aTestCase).
	self writeClosingTag: aTestCase
]

{ #category : 'reporting' }
XmlReporter >> reportResult: aTestResult [

	"Report the execution's result of a testSuite"

	stream wrappedStream position: suitePosition.

	stream
		nextPutAll: 'failures="';
		nextPutAll: aTestResult failureCount asString;
		nextPutAll: '" ';
		nextPutAll: 'errors="';
		nextPutAll: aTestResult errorCount asString;
		nextPutAll: '" ';
		nextPutAll: 'time="';
		nextPutAll: self suiteExecutionDuration asString;
		nextPutAll: '">'.

	self finishReporting
]

{ #category : 'reporting' }
XmlReporter >> reportSuite: aTestSuite runBlock: aBlock [

	"run an report the execution's result of a testSuite"

	self startReporting: aTestSuite.
	stream
		nextPutAll: '<testsuite';
		nextPutAll: ' name="';
		nextPutAll: aTestSuite name;
		nextPutAll: '"';
		nextPutAll: ' tests="';
		print: aTestSuite tests size;
		nextPutAll: '"';
		nextPutAll: ' timestamp="';
		print: Time now;
		nextPutAll: '"';
		nextPutAll: ' >'.
	suitePosition := stream wrappedStream position - 1.
	stream
		nextPutAll: (String new: 100 withAll: $ );
		lf.
	aBlock value.
	self generateStandardPart.
	stream
		nextPutAll: '</testsuite>'
]

{ #category : 'reporting' }
XmlReporter >> startReporting: aTestSuite [

	| aFile |
	aFile := File named: aTestSuite name, '.xml'.
	aFile delete.
	stream := ZnCharacterWriteStream
		          on: (aFile writeStream
				           setToEnd;
				           yourself)
		          encoding: 'utf8'.
	stream
		nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>';
		lf
]

{ #category : 'reporting' }
XmlReporter >> tabCaracter [

	^ '    '
]

{ #category : 'reporting' }
XmlReporter >> tabCaracter2Times [

	^ self tabCaracter , self tabCaracter
]

{ #category : 'reporting' }
XmlReporter >> tabCaracter3Times [

	^ self tabCaracter , self tabCaracter , self tabCaracter
]

{ #category : 'reporting' }
XmlReporter >> writeClosingTag: aTestCase [

	stream
		nextPutAll: self tabCaracter;
		nextPutAll: '</testcase>';
		lf
]

{ #category : 'reporting' }
XmlReporter >> writeException: aTestFailure [

	stream
		nextPutAll: aTestFailure class name;
		lf;
		nextPutAll: self tabCaracter3Times;
		nextPutAll: aTestFailure messageText;
		lf.
]

{ #category : 'reporting' }
XmlReporter >> writeException: aTestFailure of: aTestCase [

	self writeException: aTestFailure.
	self writeExceptionStack: aTestFailure of: aTestCase.
]

{ #category : 'reporting' }
XmlReporter >> writeExceptionStack: anException of: aTestCase [

	| context |
	context := anException signalerContext.
	[ 
	context isNil or: [ 
		context receiver == aTestCase and: [ 
			context methodSelector == #runCase ] ] ] whileFalse: [ 
		[ 
		stream
			nextPutAll: self tabCaracter3Times;
			nextPutAll: (self encode: context printString);
			lf ] onErrorDo: [ 
			stream
				nextPutAll: 'PRINTING ERROR';
				lf ].
		context := context sender ]
]

{ #category : 'reporting' }
XmlReporter >> writeOpeningFailureTag: aTestCase withProperties: aDictionnary [

	stream nextPutAll: self tabCaracter.
	self
		writeOpeningTagOf: 'failure'
		duringTest: aTestCase
		withProperties: aDictionnary
]

{ #category : 'reporting' }
XmlReporter >> writeOpeningTag: aTestCase [

	self writeOpeningTestCaseTag: aTestCase withProperties: Dictionary new
]

{ #category : 'reporting' }
XmlReporter >> writeOpeningTagOf: aTagName duringTest: aTestCase withProperties: aDictionnary [

	stream
		nextPutAll: self tabCaracter;
		nextPutAll: '<';
		nextPutAll: aTagName;
		nextPutAll: ''.
	aDictionnary keysDo: [ :attribut | 
		stream
			nextPutAll: ' ';
			nextPutAll: attribut;
			nextPutAll: '="';
			nextPutAll: (aDictionnary at: attribut) asString;
			nextPutAll: '"' ].
	stream
		nextPutAll: '>';
		lf
]

{ #category : 'reporting' }
XmlReporter >> writeOpeningTestCaseTag: aTestCase withProperties: aDictionnary [

	self
		writeOpeningTagOf: 'testcase'
		duringTest: aTestCase
		withProperties: aDictionnary
]
