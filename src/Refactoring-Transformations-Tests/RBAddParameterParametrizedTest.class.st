Class {
	#name : 'RBAddParameterParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBAddParameterParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddParameterTransformation };
		addCase: { #rbClass -> RBAddParameterRefactoring };
		
		yourself
]

{ #category : 'accessing' }
RBAddParameterParametrizedTest >> constructor [
	^ #addParameterToMethod:in:newSelector:permutation:newArgs:
]

{ #category : 'mocking' }
RBAddParameterParametrizedTest >> rbModelForVariableTest [

	| newModel classEnvironment |
	
	classEnvironment := RBClassEnvironment classes: { RBClassDataForRefactoringTest . OrderedCollection . RBLintRuleTestData . ReRefactoring . Object}.
	newModel := self defaultNamespaceClass onEnvironment: classEnvironment.
	newModel name: 'Test'.
	
	self defineFooBarIn: newModel.

	^ newModel
]

{ #category : 'running' }
RBAddParameterParametrizedTest >> setUp [
	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'tests' }
RBAddParameterParametrizedTest >> testAddParameterAndRenameParameters [
	| oldSelector newSelector refactoring class newArg |
	oldSelector := ('called:' , 'on:') asSymbol.
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	newSelector := #called:bar:on:.
	newArg := RBArgumentName name: 'anObject' value: '#(1.0)'.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBClassDataForRefactoringTest . newSelector . #(1 -1 2) . { newArg }}.
	refactoring renameMap: (Array with: ((RBArgumentName name: 'aBlock') newName: 'aBlock1')).
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self assert: (class parseTreeForSelector: newSelector) equals: (self parseMethod: 'called: anObject bar: anObject1 on: aBlock1
	anObject printString
		traceCr;
		cr.
	aBlock1 value').
	self assert: (class parseTreeForSelector: #caller) equals: (self parseMethod: 'caller
							| anObject |
							anObject := 5.
							self
								called: anObject + 1
								bar: #(1.0) on: [^anObject]').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : 'tests' }
RBAddParameterParametrizedTest >> testAddParameterForTwoArgumentMessage [
	| oldSelector newSelector refactoring class newArg |
	oldSelector := ('called:' , 'on:') asSymbol.
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	newSelector := #called:bar:on:.
	newArg := RBArgumentName name: 'anObject' value: '#(1.0)'.
	refactoring := self createRefactoringWithModel: model andArguments:
		{oldSelector . RBClassDataForRefactoringTest . newSelector . #(1 -1 2) . { newArg }}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self assert: (class parseTreeForSelector: newSelector) equals: (self parseMethod: 'called: anObject bar: anObject1 on: aBlock
	
	anObject printString traceCr; cr.
	aBlock value').
	self assert: (class parseTreeForSelector: #caller) equals: (self parseMethod: 'caller
							| anObject |
							anObject := 5.
							self
								called: anObject + 1
								bar: #(1.0) on: [^anObject]').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : 'tests' }
RBAddParameterParametrizedTest >> testAddParameterThatReferencesABlockWithInstanceVariableReference [

	| refactoring class oldSelector newSelector newArg |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testFoo:bar:.
	newArg := RBArgumentName name: 'aFoo' value: '[ :each | each + instanceVariable ]'.
	refactoring := self createRefactoringWithModel: model andArguments:
		{oldSelector . RBClassDataForRefactoringTest . newSelector . #(1 -1) . {newArg}}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self
		assert: (class parseTreeForSelector: newSelector)
		equals: (self parseMethod: 'testFoo: anObject bar: aFoo
								^self class + anObject').
	self
		assert: (class parseTreeForSelector: #callFoo)
		equals:
		(self parseMethod:
			 'callFoo ^self testFoo: 5 bar: ([ :each | each + instanceVariable ])').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : 'tests' }
RBAddParameterParametrizedTest >> testAddParameterThatReferencesGlobalAndLiteral [

	| refactoring class oldSelector newSelector newArg |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testFoo:bar:.
	newArg := RBArgumentName name: 'anObject' value: 'OrderedCollection new: 5'.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBClassDataForRefactoringTest . newSelector . #(1 -1) . {newArg} }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self
		assert: (class parseTreeForSelector: newSelector)
		equals: (self parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self
		assert: (class parseTreeForSelector: #callFoo)
		equals: (self parseMethod:
				 'callFoo ^self testFoo: 5 bar: (OrderedCollection new: 5)').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : 'tests' }
RBAddParameterParametrizedTest >> testAddParameterThatReferencesInstanceVariable [

	| refactoring class oldSelector newSelector newArg |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testFoo:bar:.
	newArg := RBArgumentName name: 'aFoo' value: 'instanceVariable'.
	refactoring := self createRefactoringWithModel: model andArguments:
		{oldSelector . RBClassDataForRefactoringTest . newSelector . #(1 -1) . {newArg}}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self
		assert: (class parseTreeForSelector: newSelector)
		equals: (self parseMethod: 'testFoo: anObject bar: aFoo
								^self class + anObject').
	self
		assert: (class parseTreeForSelector: #callFoo)
		equals:
		(self parseMethod:
			 'callFoo ^self testFoo: 5 bar: (instanceVariable)').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : 'tests' }
RBAddParameterParametrizedTest >> testAddParameterThatReferencesModelGlobal [

	| refactoring class oldSelector newSelector newArg |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testFoo:bar:.
	newArg := RBArgumentName name: 'anObject' value:  'Bar new'.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBClassDataForRefactoringTest . newSelector . #(1 -1) . {newArg}}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self
		assert: (class parseTreeForSelector: newSelector)
		equals: (self parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self
		assert: (class parseTreeForSelector: #callFoo)
		equals:
		(self parseMethod: 'callFoo ^self testFoo: 5 bar: (Bar new)').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : 'tests' }
RBAddParameterParametrizedTest >> testAddParameterThatReferencesSelf [

	| refactoring class oldSelector newSelector newArg |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testFoo:bar:.
	newArg := RBArgumentName name: 'aFoo' value: 'self printString'.
	refactoring := self createRefactoringWithModel: model andArguments:
		{oldSelector . RBClassDataForRefactoringTest . newSelector . #(1 -1) . {newArg}}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self
		assert: (class parseTreeForSelector: newSelector)
		equals: (self parseMethod: 'testFoo: anObject bar: aFoo
								^self class + anObject').
	self
		assert: (class parseTreeForSelector: #callFoo)
		equals:
		(self parseMethod:
			 'callFoo ^self testFoo: 5 bar: (self printString)').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : 'tests' }
RBAddParameterParametrizedTest >> testAddTwoParameters [

	| refactoring class oldSelector newSelector newArgs |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testColl:foo:string:.
	newArgs := Array with: (RBArgumentName name: 'aColl' value: 'OrderedCollection new: 5')
		with: (RBArgumentName name: 'aString' value: '''string''').
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBClassDataForRefactoringTest . newSelector . #(-1 1 -2) . newArgs }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self
		assert: (class parseTreeForSelector: newSelector)
		equals: (self parseMethod: 'testColl: aColl foo: anObject string: aString
								^self class + anObject').
	self
		assert: (class parseTreeForSelector: #callFoo)
		equals: (self parseMethod:
				 'callFoo ^self testColl: (OrderedCollection new: 5) foo: 5 string: ''string''').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : 'failure tests' }
RBAddParameterParametrizedTest >> testFailureBadDefaultValueForNewArgument [

	| newArg |
	newArg := RBArgumentName name: 'anObject' value: 'brokenExpression:'.
	self shouldFail: (self createRefactoringWithModel: model andArguments: {
				 #name.
				 RBLintRuleTestData.
				 #name:.
				 #( -1 ).
				 { newArg } }).
	
	newArg := RBArgumentName name: 'anObject' value: 'undefinedVariable'.
	self shouldFail: (self createRefactoringWithArguments: {
				 #name.
				 RBLintRuleTestData.
				 #name:.
				 #( -1 ).
				 { newArg } })
]

{ #category : 'failure tests' }
RBAddParameterParametrizedTest >> testFailureInvalidNumArgsOfNewSelector [

	| newArg |
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	self shouldFail: (self createRefactoringWithModel: model andArguments: {
				 #checkSendersAccessTo:.
				 RBLintRuleTestData.
				 #checkSendersAccessTo:.
				 #( 1 -1 ).
				 { newArg } })
]

{ #category : 'failure tests' }
RBAddParameterParametrizedTest >> testFailureModelBadInitializationCode [

	| refactoring newArg |
	newArg := RBArgumentName
		          name: 'anObject'
		          value: 'AddParameterRefactoring new'.
	model removeClassNamed: #ReRefactoring.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               #name1.
				               RBLintRuleTestData.
				               #name1:.
				               #( -1 ).
				               { newArg } }.
	self shouldFail: refactoring
]

{ #category : 'failure tests' }
RBAddParameterParametrizedTest >> testFailureModelNonExistantName [

	| refactoring newArg |
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	(model classNamed: #RBLintRuleTestData) removeMethod: #name.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               #name.
				               RBLintRuleTestData.
				               #nameNew:.
				               #( -1 ).
				               { newArg } }.
	self shouldFail: refactoring
]

{ #category : 'failure tests' }
RBAddParameterParametrizedTest >> testFailureNonExistantName [

	| newArg |
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	self shouldFail: (self createRefactoringWithArguments: {
				 #name1.
				 RBLintRuleTestData.
				 #name1:.
				 #( -1 ).
				 { newArg } })
]

{ #category : 'failure tests' }
RBAddParameterParametrizedTest >> testFailurePrimitiveMethods [

	| refactoring newArg |
	newArg := RBArgumentName name: 'anObject' value: '1'.
	(model classNamed: #Object)
		compile: 'foo <primitive: 100> ^#() primitiveFailed'
		classified: #( #accessing ).
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               #foo.
				               Object.
				               #foo123124321s:.
				               #( -1 ).
				               { newArg } }.
	self shouldFail: refactoring.
	refactoring := self createRefactoringWithArguments: {
			               #at:.
			               Object.
			               #at:foo:.
			               #( 1 -1 ).
			               { newArg } }.
	self shouldFail: refactoring
]

{ #category : 'failure tests' }
RBAddParameterParametrizedTest >> testFailureUseExistingNewSelector [

	| newArg |
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	self shouldFail: (self createRefactoringWithArguments: {
				 #checkSendersAccessTo:.
				 RBLintRuleTestData.
				 #safeVariableNameFor:temporaries:.
				 #( 1 -1 ).
				 { newArg } })
]
