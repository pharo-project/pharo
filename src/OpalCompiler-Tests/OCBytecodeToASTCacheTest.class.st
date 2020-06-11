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
OCBytecodeToASTCacheTest >> testNodeForBCOffsetTest [
	| pc mappedNode expectedNode |
	"we skip for now on 32 bit"
	Smalltalk vm is32bit ifTrue: [ self skip ]. 
	pc := compiledMethod encoderClass = EncoderForSistaV1 ifTrue: [50] ifFalse: [51].
	mappedNode := (cache nodeForPC: pc).
	expectedNode := compiledMethod ast statements last arguments first statements first.
	self assert: mappedNode sourceCode equals: expectedNode sourceCode.	
	self assert: mappedNode start equals: expectedNode start.
	self assert: mappedNode stop equals: expectedNode stop
]
