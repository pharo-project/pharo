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
OCBytecodeToASTCacheTest >> testHigherThanLastBCOffsetAccessTest [
	| pc |
	pc := cache lastBcOffset + 5.
	"if we are beyond the last bc, we map to the whole method"
	self
		assert: (cache nodeForPC: pc)
		identicalTo: compiledMethod ast
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
	we have to check that associations are all pointing to a node"

	| pcRange |
	pcRange := 0 to: cache lastBcOffset.
	pcRange do: [ :pc | self assert: ((cache nodeForPC: pc) isKindOf: RBProgramNode)]
]

{ #category : #tests }
OCBytecodeToASTCacheTest >> testNodeForBCOffsetTest [
	| pc |
	"there should be an assignment at pc 51"
	pc := 51.
	self assert: (cache nodeForPC: pc) isAssignment
]
