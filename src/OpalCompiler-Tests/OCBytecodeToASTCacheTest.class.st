Class {
	#name : #OCBytecodeToASTCacheTest,
	#superclass : #TestCase,
	#instVars : [
		'cache',
		'compiledMethod'
	],
	#category : #'OpalCompiler-Tests-Bytecode'
}

{ #category : #running }
OCBytecodeToASTCacheTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.	
	compiledMethod := MethodMapExamples >> #helperMethod12.
	cache := OCBytecodeToASTCache generateForMethodNode: compiledMethod ast
]

{ #category : #helpers }
OCBytecodeToASTCacheTest >> testCacheInInterval: interval equalsNode: aNode [
	interval do: [ :i | 
		self assert: (cache nodeForPC: i) identicalTo: aNode ]
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testCachedMethodNode [
	self assert: cache methodNode identicalTo: compiledMethod ast
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testFirstBCOffsetTest [
	self assert: cache firstBcOffset equals: compiledMethod initialPC
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testFirstBCOffsetWithQuickReturn [
	compiledMethod := ( MethodMapExamples >> #ivar).
	cache := OCBytecodeToASTCache generateForMethodNode: compiledMethod ast.	
	self assert: cache firstBcOffset equals: compiledMethod initialPC.
	self assert: (cache nodeForPC: cache firstBcOffset) identicalTo: compiledMethod ast.
	self assert: (cache nodeForPC: cache lastBcOffset) identicalTo: compiledMethod ast statements last.

	cache firstBcOffset to: cache lastBcOffset - 1 do:[:pc|
		self assert: (cache nodeForPC: pc) identicalTo: compiledMethod ast].
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testHigherThanLastBCOffsetAccessTest [
	| pc |
	pc := cache lastBcOffset + 5.
	self assert: (cache nodeForPC: pc) identicalTo: compiledMethod ast
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testLastBCOffsetTest [
	self
		assert: cache lastBcOffset
		equals:
			compiledMethod ast ir startSequence withAllSuccessors last last
				bytecodeOffset
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testLowerThanFirstBCOffsetAccessTest [
	self
		testCacheInInterval: (0 to: cache firstBcOffset - 1)
		equalsNode: compiledMethod ast
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testNodeForBCOffsetRangeTest [
	"As we associate each node to each possible bytecode offset that can refer to it,
	we have to check that associations are consistent between pc ranges and mapped nodes.
	In this test we check for a sample method that the mapping for **each** pc is correct.

	This test was generated using the old IRMethod>>#instructionForPC: behavior which was correct."
	|node|
	node := cache nodeForPC: 0.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 1.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 2.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 3.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 4.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 5.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 6.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 7.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 8.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 9.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 10.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 11.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 12.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 13.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 14.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 15.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 16.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 17.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 18.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 19.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 20.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 21.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 22.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 23.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 24.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 25.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 26.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 27.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 28.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 29.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 30.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 31.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 32.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 33.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 34.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 35.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 36.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 37.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 38.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 39.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 40.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

	node := cache nodeForPC: 41.
	self assert: node class equals: RBLiteralValueNode.
	self assert: node start equals: 29.
	self assert: node stop equals: 29.
	self assert: node sourceCode equals: '5'.

	node := cache nodeForPC: 42.
	self assert: node class equals: RBAssignmentNode.
	self assert: node start equals: 24.
	self assert: node stop equals: 29.
	self assert: node sourceCode equals: 'i := 5'.

	node := cache nodeForPC: 43.
	self assert: node class equals: RBTemporaryNode.
	self assert: node start equals: 35.
	self assert: node stop equals: 35.
	self assert: node sourceCode equals: 'i'.

	node := cache nodeForPC: 44.
	self assert: node class equals: RBLiteralValueNode.
	self assert: node start equals: 37.
	self assert: node stop equals: 37.
	self assert: node sourceCode equals: '0'.

	node := cache nodeForPC: 45.
	self assert: node class equals: RBMessageNode.
	self assert: node start equals: 35.
	self assert: node stop equals: 37.
	self assert: node sourceCode equals: 'i=0'.

	node := cache nodeForPC: 46.
	self assert: node class equals: RBMessageNode.
	self assert: node start equals: 33.
	self assert: node stop equals: 66.
	self assert: node sourceCode equals: '[ i=0 ] whileFalse: [ i := i - 1 ]'.

	node := cache nodeForPC: 47.
	self assert: node class equals: RBMessageNode.
	self assert: node start equals: 33.
	self assert: node stop equals: 66.
	self assert: node sourceCode equals: '[ i=0 ] whileFalse: [ i := i - 1 ]'.

	node := cache nodeForPC: 48.
	self assert: node class equals: RBTemporaryNode.
	self assert: node start equals: 60.
	self assert: node stop equals: 60.
	self assert: node sourceCode equals: 'i'.

	node := cache nodeForPC: 49.
	self assert: node class equals: RBLiteralValueNode.
	self assert: node start equals: 64.
	self assert: node stop equals: 64.
	self assert: node sourceCode equals: '1'.

	node := cache nodeForPC: 50.
	self assert: node class equals: RBMessageNode.
	self assert: node start equals: 60.
	self assert: node stop equals: 64.
	self assert: node sourceCode equals: 'i - 1'.

	node := cache nodeForPC: 51.
	self assert: node class equals: RBAssignmentNode.
	self assert: node start equals: 55.
	self assert: node stop equals: 64.
	self assert: node sourceCode equals: 'i := i - 1'.

	node := cache nodeForPC: 52.
	self assert: node class equals: RBMessageNode.
	self assert: node start equals: 33.
	self assert: node stop equals: 66.
	self assert: node sourceCode equals: '[ i=0 ] whileFalse: [ i := i - 1 ]'.

	node := cache nodeForPC: 53.
	self assert: node class equals: RBMessageNode.
	self assert: node start equals: 33.
	self assert: node stop equals: 66.
	self assert: node sourceCode equals: '[ i=0 ] whileFalse: [ i := i - 1 ]'.

	node := cache nodeForPC: 54.
	self assert: node class equals: RBMethodNode.
	self assert: node start equals: 1.
	self assert: node stop equals: 67.
	self assert: node sourceCode equals: compiledMethod ast sourceCode.

]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testNodeForBCOffsetTest [
	| pc mappedNode expectedNode |
	pc := 51.
	mappedNode := (cache nodeForPC: pc).
	expectedNode := compiledMethod ast statements last arguments first statements first.
	self assert: mappedNode sourceCode equals: expectedNode sourceCode.	
	self assert: mappedNode start equals: expectedNode start.
	self assert: mappedNode stop equals: expectedNode stop
]
