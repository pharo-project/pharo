"
Report tests execution result under XML format
"
Class {
	#name : #XmlReporter,
	#superclass : #AbstractReporter,
	#instVars : [
		'stream',
		'suitePosition'
	],
	#category : #'TestRunner-Core-Reporter'
}

{ #category : #reporting }
XmlReporter class >> generateTesCaseClassName: aTestCase [

	^ (aTestCase class category copyReplaceAll: '-' with: '.') , '.'
	  , aTestCase class name
]

{ #category : #reporting }
XmlReporter >> finishReporting [

	stream close
]

{ #category : #reporting }
XmlReporter >> generateTesCaseClassName: aTestCase [

	self class generateTesCaseClassName: aTestCase
]

{ #category : #reporting }
XmlReporter >> reportCase: aTestCase runBlock: aBlock [

	"run an report the execution's result of a testCase"

	super reportCase: aTestCase runBlock: aBlock.
	stream
		nextPutAll: '    ';
		nextPutAll: '<testcase';
		nextPutAll: ' classname="';
		nextPutAll: (self generateTesCaseClassName: aTestCase);
		nextPutAll: '"';
		nextPutAll: ' time="';
		nextPutAll: currentCaseDuration asString;
		nextPutAll: '"';
		nextPutAll: ' name="';
		nextPutAll: aTestCase nameForReport;
		nextPutAll: '">';
		nextPutAll: '</testcase>';
		lf
]

{ #category : #reporting }
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
		nextPutAll: 'time="?">'.

	self finishReporting
]

{ #category : #reporting }
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
	stream
		nextPutAll: '</testsuite>';
		lf
]

{ #category : #reporting }
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
