"
A RTFLeadsToAssertPrimitiveCallCheckerTest is a test class for testing the behavior of RTFLeadsToAssertPrimitiveCallChecker
"
Class {
	#name : 'RTFLeadsToAssertPrimitiveCallCheckerTest',
	#superclass : 'TestCase',
	#instVars : [
		'tree1',
		'tree2'
	],
	#category : 'RottenTestsFinder-Tests-SelfInterpreter',
	#package : 'RottenTestsFinder-Tests',
	#tag : 'SelfInterpreter'
}

{ #category : 'running' }
RTFLeadsToAssertPrimitiveCallCheckerTest >> setUp [
	super setUp.
	tree1 := RTFSelfCallRootNode new
				compiledMethod: (RTFFakeTestClassTest lookupSelector: #testWithHelperHelper);
				subCalls: {
					RTFSelfCallRootNode new
						compiledMethod: (RTFFakeTestClassTest lookupSelector: #helperHelper);
						subCalls: {
							RTFSelfCallRootNode new
								compiledMethod: (RTFFakeTestClassTest lookupSelector: #assertHelper);
								subCalls: {
									RTFSelfCallRootNode new
										compiledMethod: (TestAsserter lookupSelector: #assert:);
										subCalls: #()
									};
								yourself
							};
						yourself
					};
				yourself.
	tree2 := RTFSelfCallRootNode new
				compiledMethod: (RTFFakeTestClassTest lookupSelector: #testWithNoAssertionInHelperRun);
				subCalls: {
					RTFSelfCallRootNode new
						compiledMethod: (RTFFakeTestClassTest lookupSelector: #rottenHelper);
						subCalls: #();
						yourself
					};
				yourself
]

{ #category : 'tests-visiting' }
RTFLeadsToAssertPrimitiveCallCheckerTest >> testVisitRTFSelfCallNode [
	self
		assert: (RTFLeadsToAssertPrimitiveCallChecker new visitRTFSelfCallNode: tree1);
		deny: (RTFLeadsToAssertPrimitiveCallChecker new visitRTFSelfCallNode: tree2)
]
