"
A RTFSelfCallTreeCleanerTest is a test class for testing the behavior of RTFSelfCallTreeCleaner
"
Class {
	#name : 'RTFSelfCallTreeCleanerTest',
	#superclass : 'TestCase',
	#instVars : [
		'tree1',
		'tree2'
	],
	#category : 'RottenTestsFinder-Tests',
	#package : 'RottenTestsFinder-Tests'
}

{ #category : 'running' }
RTFSelfCallTreeCleanerTest >> setUp [
	super setUp.
	tree1 := RTFSelfCallRootNode new
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
				yourself.
	tree2 := RTFSelfCallRootNode new
				compiledMethod: 'Fake CM';
				subCalls: {
					RTFSelfCallRootNode new
						compiledMethod: 'Fake CM 2';
						subCalls: #();
						yourself
					};
				yourself
]

{ #category : 'tests-visiting' }
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
