Class {
	#name : 'ReSemanticsOfInlineMethodRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'utilities' }
ReSemanticsOfInlineMethodRefactoringTest >> inlineMessageSend: messageSendSource inlineMethodSource:  methodToInlineSource ofMethodSource: source [

	| method methodToInline refactoring |
	method := class compile: source classified: '#test data'.
	methodToInline := class compile: methodToInlineSource classified: '#test data'.

	refactoring := RBInlineMethodRefactoring
		  model: model
		  inlineSource: messageSendSource 
		  fromMethod: method selector
		  inClass: class name.
		
	self setupInlineExpressionFor: refactoring toReturn: false.
	
	^ refactoring 
]

{ #category : 'running' }
ReSemanticsOfInlineMethodRefactoringTest >> setUp [

	| package |
	super setUp.
	package := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	model := RBNamespace onEnvironment: package.
	model defineClass: [ :aBuilder | 
		aBuilder
			superclass: Object;
			name: #ReClassForTesting;
			slots: { #instVar . #instVar2 };
			package: 'Refactoring-DataForTesting'].
	class := model classNamed: 'ReClassForTesting'
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInAssignmentWhenInlineMethodWithIfTrueAndReturnExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar ifTrue: [ ^ 2 ].  ^ 1 '
		ofMethodSource: 'm instVar := self foo'.
		
	refactoring generateChanges.
	
	self
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := instVar ifTrue: [ 2 ] ifFalse: [ 1 ]')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInAssignmentWhenInlineMethodWithIfTrueIfFalseExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar ifTrue: [ ^ 2 ] ifFalse: [ ^ 1 ]'
		ofMethodSource: 'm instVar := self foo'.
		
	refactoring generateChanges.
	
	self
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := instVar ifTrue: [ 2 ] ifFalse: [ 1 ]')
]

{ #category : 'tests - error' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInAssignmentWhenInlineMethodWithNonLocalReturningBlockAndRetunExpectError [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar test: [ ^ 2 ]. ^ 1'
		ofMethodSource: 'm instVar := self foo'.
		
	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringError
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInBlockWithReturnWhenInlineMethodHasNonLocalReturningBlockExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar ifTrue: [ ^ 2 ] '
		ofMethodSource: 'm instVar := [ ^ self foo ]'.
		
	refactoring generateChanges.
	
	self
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := [ instVar ifTrue: [ ^ 2 ]. ^ self ]')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInReturnNodeWhenInlineMethodWithArgumentUsedMultipleTimes [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: (1 + 1)'
		inlineMethodSource:  'foo: arg ^ arg + arg'
		ofMethodSource: 'm ^ self foo: (1 + 1)'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | arg | arg := 1 + 1. ^ arg + arg')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInReturnNodeWhenInlineMethodWithExpression [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo ^ 1 + 1'
		ofMethodSource: 'm ^ self foo'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm ^ 1 + 1')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInReturnNodeWhenInlineMethodWithIfTrueIfFalseExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar ifTrue: [ ^ 1 ] ifFalse: [ ^ 3 ]'
		ofMethodSource: 'm ^ self foo'.
		
	refactoring generateChanges.
	self flag: #todo. "This is strage result even though it is correct one. Why it removed return node?"
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar ifTrue: [ ^ 1 ] ifFalse: [ ^ 3 ]')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInReturnNodeWhenInlineMethodWithIfTrueReturnsLiteral [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar ifTrue: [ ^ 2 ]'
		ofMethodSource: 'm ^ self foo'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar ifTrue: [ ^ 2 ]. ^ self')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInReturnNodeWhenInlineMethodWithNonLocalReturningBlock [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar test: [ ^ 2 ]'
		ofMethodSource: 'm ^ self foo'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar test: [ ^ 2 ]. ^ self')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInReturnNodeWhenInlineMethodWithNonLocalReturningBlockAndRetun [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar test: [ ^ 2 ]. ^ 1'
		ofMethodSource: 'm ^ self foo'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar test: [ ^ 2 ]. ^ 1')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInReturnNodeWhenInlineMethodWithReturnIfTrueIfFalseExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo ^ instVar ifTrue: [ ^ 1 ] ifFalse: [ ^ 3 ]'
		ofMethodSource: 'm ^ self foo'.
		
	refactoring generateChanges.
	self flag: #todo. "This is strage result even though it is correct one"
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm ^ instVar ifTrue: [ ^ 1 ] ifFalse: [ ^ 3 ]')
]

{ #category : 'tests - error' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInStatementWhenInlineMethodWithNonLocalReturningBlockAndReturnAndStatmentsAfterInlined [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar test: [ ^ self bar ]. ^ self zed'
		ofMethodSource: 'm self foo. self bar'.
	
	self
		should: [ refactoring generateChanges ]	
		raise: RBRefactoringError
]

{ #category : 'tests - error' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInStatementWhenInlineMethodWithNonLocalReturningBlockStatmentsAfterInlined [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar test: [ ^ self bar ]'
		ofMethodSource: 'm self foo. self bar'.
	
	self
		should: [ refactoring generateChanges ]	
		raise: RBRefactoringError
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInStatementWhenInlineMethodWithReturnIfTrueIfFalseExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo ^ instVar ifTrue: [ ^ 1 ] ifFalse: [ ^ 1 ]'
		ofMethodSource: 'm self foo. self bar'.
		
	refactoring generateChanges.
	self flag: #todo. "a ifFalse: [] can be removed since it is also a dead code."
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar ifFalse: [ ]. self bar')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInStatementWhenInlineMethodWithReturnIfTrueIfFalseWithExpressionsExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo ^ instVar ifTrue: [ ^ 1 + 1 ] ifFalse: [ ^ 3 + 1 ]'
		ofMethodSource: 'm self foo. self bar'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar ifTrue: [ 1 + 1 ] ifFalse: [ 3 + 1 ]. self bar')
]

{ #category : 'tests - control flow' }
ReSemanticsOfInlineMethodRefactoringTest >> testControlFlowInStatementWithBlockWithReturnWhenInlineMethodHasReturnExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo ^ 2 * instVar2 - 1 '
		ofMethodSource: 'm instVar := [ instVar2 := self foo ]. self bar'.
		
	refactoring generateChanges.
	
	self
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := [ instVar2 := 2 * instVar2 - 1 ]. self bar')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodExpressionThatIsNotDeadCode [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo Point new'
		ofMethodSource: 'm self foo'.
		
	refactoring generateChanges.
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm Point new')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodInTheMiddleOfSequenceExpectSameControlFlow [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar := Point new'
		ofMethodSource: 'm instVar := 1. self foo. instVar2 := 1'.
		
	refactoring generateChanges.
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := 1. instVar := Point new. instVar2 := 1')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodInlineRemovesDeadCode [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo ^ 3'
		ofMethodSource: 'm self foo'.
		
	refactoring generateChanges.
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodInlinesLiteral [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo ^ 3'
		ofMethodSource: 'm ^ self foo'.
		
	refactoring generateChanges.
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm ^ 3')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithInstanceVariableReferences [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo instVar := Point new'
		ofMethodSource: 'm self foo'.
		
	refactoring generateChanges.
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := Point new')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithTempsNestedWithLocalConflictExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo | x | x := Point new'
		ofMethodSource: 'm | x | x := [ | x1 | self foo ]'.
		
	refactoring generateChanges.
	self flag: #unnecessarySelfFromInlinedMethod.
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | x | x := [ | x1 x2 | x2 := Point new. self ]')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithTempsNestedWithoutConflictExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo | x | x := Point new'
		ofMethodSource: 'm | y | y := [ | x1 | self foo ]'.
		
	refactoring generateChanges.
	self flag: #unnecessarySelfFromInlinedMethod.
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | y | y := [ | x1 x | x := Point new. self ]')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithTempsWithConflictExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo | x | x := Point new'
		ofMethodSource: 'm | x | x := 1. self foo'.
		
	refactoring generateChanges.
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | x x1 | x := 1. x1 := Point new')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithTempsWithTwoConflictsExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo | x | x := Point new'
		ofMethodSource: 'm | x x1 | x := 1. x1 := 2. self foo'.
		
	refactoring generateChanges.
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | x x1 x2 | x := 1. x1 := 2. x2 := Point new')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithTempsWithinBlockWithoutConflictExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo | x | x := Point new'
		ofMethodSource: 'm instVar := [ | x1 | self foo ]'.
		
	refactoring generateChanges.
	self flag: #unnecessarySelfFromInlinedMethod.
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := [ | x1 x | x := Point new. self ]')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithTempsWithoutConflictExpectSuccess [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo'
		inlineMethodSource:  'foo | x | x := Point new'
		ofMethodSource: 'm | y | y := 1. self foo'.
		
	refactoring generateChanges.
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | y x | y := 1. x := Point new')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithoutReturnInAssignmentWithExpressionArgument [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: (1 + 1)'
		inlineMethodSource:  'foo: arg instVar := Point new + arg'
		ofMethodSource: 'm instVar2 := self foo: (1 + 1)'.

	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | arg | arg := 1 + 1. instVar := Point new + arg. instVar2 := self')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithoutReturnInReturnNodetWithExpressionArgument [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: (1 + 1)'
		inlineMethodSource:  'foo: arg instVar := Point new + arg'
		ofMethodSource: 'm ^ self foo: (1 + 1)'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | arg | arg := 1 + 1. instVar := Point new + arg. ^ self')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithoutReturnWithArgumentsConflict [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: (1 + 1)'
		inlineMethodSource:  'foo: arg instVar := Point new. self bar: arg'
		ofMethodSource: 'm: arg self foo: (1 + 1)'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m:)
		equals: (self parseMethod: 'm: arg | arg1 | arg1 := 1 + 1. instVar := Point new. self bar: arg1')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithoutReturnWithArgumentsConflictWithTemp [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: (1 + 1)'
		inlineMethodSource:  'foo: arg instVar := Point new. self bar: arg'
		ofMethodSource: 'm: arg | arg1 | arg1 := self foo: (1 + 1)'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m:)
		equals: (self parseMethod: 'm: arg | arg1 arg2 | arg2 := 1 + 1. instVar := Point new. self bar: arg2. arg1 := self')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithoutReturnWithExpressionArgumentUsedMultipleTimes [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: (1 + 1)'
		inlineMethodSource:  'foo: arg instVar := Point new + arg. self bar: arg. instVar2 := arg'
		ofMethodSource: 'm self foo: (1 + 1)'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | arg | arg := 1 + 1. instVar := Point new + arg. self bar: arg. instVar2 := arg')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithoutReturnWithLiteralArgumentUsedMultipleTimes [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: 1'
		inlineMethodSource:  'foo: arg instVar := Point new + arg. self bar: arg. instVar2 := arg'
		ofMethodSource: 'm self foo: 1'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := Point new + 1. self bar: 1. instVar2 := 1')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithoutReturnWithUnusedArgumentUsedMultipleTimes [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: 1'
		inlineMethodSource:  'foo: arg instVar := Point new. self bar'
		ofMethodSource: 'm self foo: 1'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm instVar := Point new. self bar')
]

{ #category : 'tests' }
ReSemanticsOfInlineMethodRefactoringTest >> testInlineMethodWithoutReturnWithUnusedExpressionArgumentUsedMultipleTimes [

	| refactoring |
	refactoring := self 
		inlineMessageSend: 'self foo: (1 + 1)'
		inlineMethodSource:  'foo: arg instVar := Point new. self bar'
		ofMethodSource: 'm self foo: (1 + 1)'.
		
	refactoring generateChanges.

	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm | arg | arg := 1 + 1. instVar := Point new. self bar')
]
