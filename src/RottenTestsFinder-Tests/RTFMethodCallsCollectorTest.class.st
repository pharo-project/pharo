"
A RTFMethodCallsCollectorTest is a test class for testing the behavior of RTFMethodCallsCollector
"
Class {
	#name : 'RTFMethodCallsCollectorTest',
	#superclass : 'TestCase',
	#instVars : [
		'tree'
	],
	#category : 'RottenTestsFinder-Tests',
	#package : 'RottenTestsFinder-Tests'
}

{ #category : 'running' }
RTFMethodCallsCollectorTest >> setUp [
	super setUp.
	tree := RTFSelfCallRootNode new
				compiledMethod: 'Fake CM';
				subCalls: {
					RTFSelfCallRootNode new
						compiledMethod: 'Fake CM 2';
						subCalls: {
							RTFSelfCallRootNode new
								compiledMethod: 'Fake CM 3';
								subCalls: {
									RTFSelfCallRootNode new
										compiledMethod: (TestAsserter lookupSelector: #assert:);
										subCalls: #()
									};
								yourself.
							RTFSelfCallRootNode new
								compiledMethod: 'Fake CM 4';
								subCalls: {
									RTFSelfCallRootNode new
										compiledMethod: 'Fake CM 5';
										subCalls: { };
										yourself
									};
								yourself
							};
						yourself
					};
				yourself
]

{ #category : 'tests-visiting' }
RTFMethodCallsCollectorTest >> testVisitRTFSelfCallNode [
	| visitor methods |
	visitor := RTFMethodCallsCollector new.
	methods := tree acceptVisitor: visitor.

	self
		assertCollection: visitor compiledMethods
		hasSameElements: { 'Fake CM' . 'Fake CM 2' . 'Fake CM 3' . (TestAsserter lookupSelector: #assert:) . 'Fake CM 4' . 'Fake CM 5' }
]
