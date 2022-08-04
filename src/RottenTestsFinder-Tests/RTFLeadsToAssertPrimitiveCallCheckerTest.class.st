"
A RTFLeadsToAssertPrimitiveCallCheckerTest is a test class for testing the behavior of RTFLeadsToAssertPrimitiveCallChecker
"
Class {
	#name : #RTFLeadsToAssertPrimitiveCallCheckerTest,
	#superclass : #TestCase,
	#instVars : [
		'tree1',
		'tree2'
	],
	#category : #'RottenTestsFinder-Tests'
}

{ #category : #'tests-visiting' }
RTFLeadsToAssertPrimitiveCallCheckerTest >> testVisitRTFSelfCallNode [
	self
		assert: (RTFLeadsToAssertPrimitiveCallChecker new visitRTFSelfCallNode: tree1);
		deny: (RTFLeadsToAssertPrimitiveCallChecker new visitRTFSelfCallNode: tree2)
]
