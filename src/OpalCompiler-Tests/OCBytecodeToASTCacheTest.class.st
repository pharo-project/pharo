Class {
	#name : 'OCBytecodeToASTCacheTest',
	#superclass : 'TestCase',
	#instVars : [
		'cache',
		'compiledMethod'
	],
	#category : 'OpalCompiler-Tests-Bytecode',
	#package : 'OpalCompiler-Tests',
	#tag : 'Bytecode'
}

{ #category : 'running' }
OCBytecodeToASTCacheTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	compiledMethod := MethodMapExamples >> #helperMethod12.
	cache := OCBytecodeToASTCache generateForNode: compiledMethod ast
]

{ #category : 'helpers' }
OCBytecodeToASTCacheTest >> testCacheInInterval: interval equalsNode: aNode [
	interval do: [ :i |
		self assert: (cache nodeForPC: i) identicalTo: aNode ]
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testCachedMethodNode [
	self assert: cache methodOrBlockNode identicalTo: compiledMethod ast
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testFirstBCOffsetTest [
	self assert: cache firstBcOffset equals: compiledMethod initialPC
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testFirstBCOffsetWithBlock [
	compiledMethod := (MethodMapExamples >> #helperMethod14:).
	cache := OCBytecodeToASTCache generateForNode: compiledMethod ast.
	self assert: (cache nodeForPC: cache firstBcOffset) identicalTo: compiledMethod ast statements first receiver
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testFirstBCOffsetWithQuickReturn [
	compiledMethod := ( MethodMapExamples >> #ivar).
	cache := OCBytecodeToASTCache generateForNode: compiledMethod ast.
	self assert: cache firstBcOffset equals: compiledMethod initialPC.
	self assert: (cache nodeForPC: cache firstBcOffset) identicalTo: compiledMethod ast.
	self assert: (cache nodeForPC: cache lastBcOffset) identicalTo: compiledMethod ast statements last.

	cache firstBcOffset to: cache lastBcOffset - 1 do:[:pc|
		self assert: (cache nodeForPC: pc) identicalTo: compiledMethod ast]
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testFirstBcOffsetForNodeWhenMappedPcsAreEmpty [

	| aimedNode pcsForAimedNode |
	compiledMethod := MethodMapExamples >> #helperMethod13.
	cache := OCBytecodeToASTCache generateForNode: compiledMethod ast.
	aimedNode := compiledMethod ast statements second receiver.
	pcsForAimedNode := cache pcsForNode: aimedNode.

	self assertEmpty: pcsForAimedNode.
	self assert: (cache firstBcOffsetForNode: aimedNode) equals: nil
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testFirstBcOffsetForNodeWhenMappedPcsAreNotEmpty [

	| aimedNode pcsForAimedNode |
	compiledMethod := MethodMapExamples >> #helperMethod13.
	cache := OCBytecodeToASTCache generateForNode: compiledMethod ast.
	aimedNode := compiledMethod ast statements second.
	pcsForAimedNode := cache pcsForNode: aimedNode.

	self deny: pcsForAimedNode isEmpty.
	self
		assert: (cache firstBcOffsetForNode: aimedNode)
		equals: pcsForAimedNode first
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testHigherThanLastBCOffsetAccessTest [
	| pc |
	pc := cache lastBcOffset + 5.
	"if we are beyond the last bc, we map to the whole method"
	self
		assert: (cache nodeForPC: pc)
		identicalTo: compiledMethod ast
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testLastBCOffsetTest [
	self
		assert: cache lastBcOffset
		equals:
			compiledMethod ast ir startSequence withAllSuccessors last last
				bytecodeOffset
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testLastBcOffsetForNodeWhenMappedPcsAreEmpty [

	| aimedNode pcsForAimedNode |
	compiledMethod := MethodMapExamples >> #helperMethod13.
	cache := OCBytecodeToASTCache generateForNode: compiledMethod ast.
	aimedNode := compiledMethod ast statements second receiver.
	pcsForAimedNode := cache pcsForNode: aimedNode.

	self assertEmpty: pcsForAimedNode.
	self assert: (cache lastBcOffsetForNode: aimedNode) equals: nil
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testLastBcOffsetForNodeWhenMappedPcsAreNotEmpty [

	| aimedNode pcsForAimedNode |
	compiledMethod := MethodMapExamples >> #helperMethod13.
	cache := OCBytecodeToASTCache generateForNode: compiledMethod ast.
	aimedNode := compiledMethod ast statements second.
	pcsForAimedNode := cache pcsForNode: aimedNode.

	self deny: pcsForAimedNode isEmpty.
	self
		assert: (cache lastBcOffsetForNode: aimedNode)
		equals: pcsForAimedNode last
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testLowerThanFirstBCOffsetAccessTest [
	self
		testCacheInInterval: (0 to: cache firstBcOffset - 1)
		equalsNode: compiledMethod ast
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testNodeForBCOffsetTest [
	| pc mappedNode expectedNode |
	self flag: #TODO. "we skip for now on 32 bit"
	Smalltalk vm is32bit ifTrue: [ self skip ].
	pc := 50.
	mappedNode := (cache nodeForPC: pc).
	expectedNode := compiledMethod ast statements second arguments first statements first.
	self assert: mappedNode sourceCode equals: expectedNode sourceCode.
	self assert: mappedNode start equals: expectedNode start.
	self assert: mappedNode stop equals: expectedNode stop
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testNodeForBCOffsetTestFull [
	| pc mappedNode expectedNode blockNode |
	self flag: #TODO. "we skip for now on 32 bit"
	Smalltalk vm is32bit ifTrue: [ self skip ].
	compiledMethod := MethodMapExamples >> #helperMethod13.

	"this is a test for full blocks only"
	blockNode := compiledMethod ast statements last value.
	pc := 27.
	mappedNode := (blockNode sourceNodeForPC: pc).
	"the message node"
	expectedNode := blockNode body statements first value.
	self assert: mappedNode sourceCode equals: expectedNode sourceCode.
	self assert: mappedNode start equals: expectedNode start.
	self assert: mappedNode stop equals: expectedNode stop
]

{ #category : 'tests' }
OCBytecodeToASTCacheTest >> testPcsForNode [

	| aimedNode pcsForAimedNode |
	compiledMethod := MethodMapExamples >> #helperMethod13.
	cache := OCBytecodeToASTCache generateForNode: compiledMethod ast.
	aimedNode := compiledMethod ast statements second.
	pcsForAimedNode := cache pcsForNode: aimedNode.

	pcsForAimedNode do: [ :pc |
		self assert: (cache nodeForPC: pc) identicalTo: aimedNode ].

	self assert: pcsForAimedNode isSorted.

	(cache bcToASTMap keys reject: [ :value |
		 pcsForAimedNode includes: value ]) do: [ :pc |
		self deny: (cache nodeForPC: pc) identicalTo: aimedNode ]
]
