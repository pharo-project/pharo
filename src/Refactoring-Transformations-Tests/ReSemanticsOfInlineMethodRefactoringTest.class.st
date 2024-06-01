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

	| method methodToInline |
	method := class compile: source classified: '#test data'.
	methodToInline := class compile: methodToInlineSource classified: '#test data'.

	^ RBInlineMethodRefactoring
		  model: model
		  inlineSource: messageSendSource 
		  fromMethod: method selector
		  inClass: class name
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
