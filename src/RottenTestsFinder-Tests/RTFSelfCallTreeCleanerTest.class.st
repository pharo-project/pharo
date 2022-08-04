"
A RTFSelfCallTreeCleanerTest is a test class for testing the behavior of RTFSelfCallTreeCleaner
"
Class {
	#name : #RTFSelfCallTreeCleanerTest,
	#superclass : #TestCase,
	#instVars : [
		'tree1',
		'tree2'
	],
	#category : #'RottenTestsFinder-Tests'
}

{ #category : #'tests-visiting' }
RTFSelfCallTreeCleanerTest >> testVisitRTFSelfCallNode [
	| cleanedTree1 cleanedTree2 |
	cleanedTree1 := tree1 acceptVisitor: RTFSelfCallTreeCleaner new.
	
	self
		assert: cleanedTree1 compiledMethod equals: 'Fake CM';
		assert: cleanedTree1 subCalls size equals: 1;
		assert: cleanedTree1 subCalls first compiledMethod equals: 'Fake CM 2';
		assert: cleanedTree1 subCalls first subCalls size equals: 1;
		assert: cleanedTree1 subCalls first subCalls first compiledMethod equals: 'Fake CM 3';
		assert: cleanedTree1 subCalls first subCalls first subCalls size equals: 1;
		assert: cleanedTree1 subCalls first subCalls first subCalls first compiledMethod equals: (TestAsserter lookupSelector: #assert:).
		
	cleanedTree2 :=  tree2 acceptVisitor: RTFSelfCallTreeCleaner new.
	
	self
		assert: cleanedTree2 compiledMethod equals: 'Fake CM';
		assert: cleanedTree2 subCalls size equals: 0
]
