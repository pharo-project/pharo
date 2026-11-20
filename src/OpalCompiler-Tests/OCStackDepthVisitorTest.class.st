Class {
	#name : 'OCStackDepthVisitorTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests',
	#package : 'OpalCompiler-Tests'
}

{ #category : 'tests' }
OCStackDepthVisitorTest >> analyzed: aString [

	| method |
	method := OpalCompiler new
		          source: aString;
		          parse.
	method doSemanticAnalysis.
	^ method
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> depthOf: aString [

	| method |
	method := OpalCompiler new
		          source: 'test: arg ' , aString;
		          parse.
	method doSemanticAnalysis.
	^ OCStackDepthVisitor new visitNode: method
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testArrayFirstElementDepth [

	self assert: (self depthOf: '^ { 1 + (2 + 3) . 4 }') equals: 3
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testArrayLastElementDepth [

	self assert: (self depthOf: '^ { 1 . 2 . (3 + 4) }') equals: 4
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testArrayPendingElements [

	self assert: (self depthOf: '^ { 1 . 2 . 3 }') equals: 3
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testAssignment [

	self assert: (self depthOf: '| t | t := 1 + 2. ^ t') equals: 2
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testCascade [

	self assert: (self depthOf: '^ 1 foo: 2; bar: 3') equals: 3
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testEmptyArray [

	self assert: (self depthOf: '^ { }') equals: 1
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testFullBlockPushesCopiedValues [

	self assert: (self depthOf: '| a b | a := b := 1. ^ [ a + b ]') equals: 2
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testInlinedTempCount [

	| method |
	method := self analyzed: 'test: arg arg ifTrue: [ | y z | y := z := 1. y ]. [ | skipped | skipped := 1 ] value'.
	self assert: (self translator inlinedTempCountIn: method) equals: 2
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testLiteral [

	self assert: (self depthOf: '^ 1') equals: 1
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testMaxFrameSizeCannotExceedFullFrameSize [

	| context |
	context := OCCompilationContext new.
	context maxFrameSize: CompiledCode fullFrameSize + 1.
	self assert: context maxFrameSize equals: CompiledCode fullFrameSize.
	context maxFrameSize: 10.
	self assert: context maxFrameSize equals: 10
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testMessage [

	self assert: (self depthOf: '^ 1 + 2') equals: 2
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testNestedArgument [

	self assert: (self depthOf: '^ 1 + (2 + (3 + 4))') equals: 4
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testNestedReceiverDoesNotAccumulate [

	self assert: (self depthOf: '^ ((1 + 2) + 3) + 4') equals: 2
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testOwnTempCountOfFullBlock [

	| block |
	block := (self analyzed: 'test: arg ^ [ :x | | y | y := x. y + arg ]') blockNodes first.
	self assert: (self translator ownTempCountOf: block) equals: 3
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testOwnTempCountOfMethod [

	self assert: (self translator ownTempCountOf: (self analyzed: 'test: arg | a b | a := b := arg. ^ a')) equals: 3
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testPlanArraysInRealMethods [

	{
		OCASTTranslator.
		OCASTSemanticAnalyzer.
		OCMethodScope.
		Object.
		Array.
		RFASTTranslator } do: [ :class |
			class methods do: [ :method |
					| ast |
					ast := method ast.
					self translator planArraysIn: ast.
					ast blockNodes do: [ :block | block isInlined ifFalse: [ self translator planArraysIn: block ] ] ] ]
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testPlannerClearsStaleDecision [

	| method array |
	method := self analyzed: 'test ^ { 1 . 2 }'.
	array := method body statements first value.
	array propertyAt: #compileAsLargeArray put: true.
	(OCArrayCompilationPlanner new limit: 5) visitNode: method body.
	self deny: (array hasProperty: #compileAsLargeArray)
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testPlannerMarksOverflowingArrayOnly [

	| method outer inner |
	method := self analyzed: 'test ^ { 1 . 2 . 3 . 4 . { 5 . 6 } }'.
	(OCArrayCompilationPlanner new limit: 5) visitNode: method body.
	outer := method body statements first value.
	inner := outer statements last.
	self assert: (outer hasProperty: #compileAsLargeArray).
	self deny: (inner hasProperty: #compileAsLargeArray)
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testSequenceTakesDeepestStatement [

	self assert: (self depthOf: '1 + 2. { 1 . 2 . 3 }. ^ 1') equals: 3
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> testVariable [

	self assert: (self depthOf: '^ arg') equals: 1
]

{ #category : 'tests' }
OCStackDepthVisitorTest >> translator [

	^ OCASTTranslator new compilationContext: OpalCompiler new compilationContext
]
