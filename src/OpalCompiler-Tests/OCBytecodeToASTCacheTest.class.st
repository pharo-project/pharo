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
	self
		assert: (cache nodeForPC: pc)
		identicalTo: (compiledMethod sourceNodeForPC: pc)
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
	we have to check that associations are consistent between a range and a node"

	| pcRange |
	pcRange := 0 to: cache lastBcOffset.
	pcRange do: [ :pc | 
		self
			assert: (cache nodeForPC: pc)
			identicalTo: (compiledMethod sourceNodeForPC: pc) ]
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testNodeForBCOffsetTest [
	| pc |
	pc := 51.
	self
		assert: (cache nodeForPC: pc)
		identicalTo: (compiledMethod sourceNodeForPC: pc)
]
