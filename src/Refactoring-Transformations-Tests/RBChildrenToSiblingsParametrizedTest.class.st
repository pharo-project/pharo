Class {
	#name : 'RBChildrenToSiblingsParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'accessing' }
RBChildrenToSiblingsParametrizedTest >> constructor [
	^ #name:class:subclasses:
]

{ #category : 'running' }
RBChildrenToSiblingsParametrizedTest >> setUp [

	super setUp.
	rbClass := RBChildrenToSiblingsRefactoring.

	(model := RBNamespace onEnvironment: ((RBClassEnvironment onEnvironment: RBBrowserEnvironment new) classes:
			           (#( #ConcreteSubclass #ConcreteSuperclass #NoMoveSubclass ) inject: OrderedCollection new into: [ :sum :each |
				            Smalltalk at: each ifPresent: [ :class | sum add: class ].
				            sum ]) , (#( #ConcreteSubclass #ConcreteSuperclass #NoMoveSubclass ) inject: OrderedCollection new into: [ :sum :each |
				            Smalltalk at: each ifPresent: [ :class | sum add: class class ].
				            sum ])) not) name: 'Test'.

	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #ConcreteSuperclass;
				slots: { #instVarName1. #instVarName2 };
				sharedVariables: { #ClassVarName1. #ClassVarName2 };
				package: #Testing ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #ConcreteSuperclass;
				name: #ConcreteSubclass;
				package: #Testing ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #ConcreteSuperclass;
				name: #NoMoveSubclass;
				package: #Testing ].
	(model metaclassNamed: #ConcreteSuperclass) addInstanceVariable: 'classInstVarName1'.
	#( #( #ConcreteSubclass #( #( 'initialize
	super initialize.
	instVarName1 := nil' #everyone ) #( 'different
	^0' #everyone ) ) ) #( #ConcreteSuperclass #( #( 'same
	^self initialize isKindOf: ConcreteSuperclass' #'one def' ) #( 'different
	^instVarName1 + instVarName2' #everyone ) #( 'initialize
	instVarName1 := instVarName2 := ClassVarName1 := ClassVarName2 := 0' #everyone ) ) ) #( #NoMoveSubclass #( #( 'same
	^123' #'one def' ) ) ) #( 'ConcreteSubclass class' #( #( 'bar
	^self storeString' #testing ) ) ) #( 'ConcreteSuperclass class' #( #( 'foo
	^classInstVarName1 + ClassVarName1 + ClassVarName2' #testing ) #( 'new
	^super new initialize' #testing ) #( 'bar
	^self printString' #testing ) ) ) #( 'NoMoveSubclass class' #(  ) ) ) do: [ :each |
		| class |
		class := model classNamed: each first.
		each last do: [ :meth | class compile: meth first classified: meth last ] ]
]

{ #category : 'failure tests' }
RBChildrenToSiblingsParametrizedTest >> testFailureBadName [

	self shouldFail: (self createRefactoringWithArguments:
		{#'Obje ct' . RBLintRuleTestData . (Array
					with: RBBasicLintRuleTestData
					with: RBCompositeLintRuleTestData)})
]

{ #category : 'failure tests' }
RBChildrenToSiblingsParametrizedTest >> testFailureExistingName [

	self shouldFail: (self createRefactoringWithArguments: {
				 #Object.
				 RBLintRuleTestData.
				 (Array
					  with: RBBasicLintRuleTestData
					  with: RBCompositeLintRuleTestData) }).
	self shouldFail: (self createRefactoringWithArguments: {
				 #Processor.
				 RBLintRuleTestData.
				 (Array
					  with: RBBasicLintRuleTestData
					  with: RBCompositeLintRuleTestData) })
]

{ #category : 'failure tests' }
RBChildrenToSiblingsParametrizedTest >> testFailureInvalidSubclass [

	self shouldFail: (self createRefactoringWithArguments: {
				 #Foo.
				 RBAbstractRefactoringTest.
				 (Array
					  with: RBBasicLintRuleTestData
					  with: RBCompositeLintRuleTestData) })
]

{ #category : 'failure tests' }
RBChildrenToSiblingsParametrizedTest >> testFailureMetaClassFailure [

	self shouldFail: (self createRefactoringWithArguments: {
				 #Foo.
				 RBLintRuleTestData class.
				 (Array
					  with: RBBasicLintRuleTestData class
					  with: RBCompositeLintRuleTestData class) })
]

{ #category : 'tests' }
RBChildrenToSiblingsParametrizedTest >> testModelChildrenToSibling [
	| refactoring class subclass superclass |
	class := model classNamed: #ConcreteSuperclass.
	subclass := model classNamed: #ConcreteSubclass.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ #AbstractSuperclass . class . (Array with: subclass)}.
	self executeRefactoring: refactoring.
	superclass := refactoring model classNamed: #AbstractSuperclass.
	self assert: class superclass equals: superclass.
	self assert: class classSide superclass equals: superclass classSide.
	self assert: subclass superclass equals: superclass.
	self assert: subclass classSide superclass equals: superclass classSide.
	self assert: (superclass parseTreeForSelector: #same) equals: (self parseMethod: 'same ^self initialize isKindOf: AbstractSuperclass').
	self assert: (superclass parseTreeForSelector: #different) equals: (self parseMethod: 'different self subclassResponsibility').
	self
		assert: (superclass parseTreeForSelector: #initialize)
		equals:
			(self
				parseMethod:
					'initialize
							instVarName1 := instVarName2 := ClassVarName1 := ClassVarName2 := 0').
	self assert: (superclass directlyDefinesInstanceVariable: 'instVarName1').
	self assert: (superclass directlyDefinesInstanceVariable: 'instVarName2').
	self assert: (superclass directlyDefinesClassVariable: 'ClassVarName1').
	self assert: (superclass directlyDefinesClassVariable: 'ClassVarName2').
	self assert: (superclass classSide directlyDefinesInstanceVariable: 'classInstVarName1').
	self
		assert: (superclass classSide parseTreeForSelector: #foo)
		equals:
			(self
				parseMethod:
					'foo
							^classInstVarName1 + ClassVarName1 + ClassVarName2').
	self
		assert: (superclass classSide parseTreeForSelector: #new)
		equals:
			(self
				parseMethod:
					'new
							^super new initialize').
	self
		assert: (superclass classSide parseTreeForSelector: #bar)
		equals:
			(self
				parseMethod:
					'bar
							self subclassResponsibility').
	self deny: (class directlyDefinesInstanceVariable: 'instVarName1').
	self deny: (class directlyDefinesInstanceVariable: 'instVarName2').
	self deny: (class directlyDefinesClassVariable: 'ClassVarName1').
	self deny: (class directlyDefinesClassVariable: 'ClassVarName2').
	self deny: (class classSide directlyDefinesInstanceVariable: 'classInstVarName1').
	self deny: (class directlyDefinesMethod: #same).
	self deny: (class directlyDefinesMethod: #initialize).
	self deny: (class classSide directlyDefinesMethod: #new).
	self
		assert: (class parseTreeForSelector: #different)
		equals:
			(self
				parseMethod:
					'different
							^instVarName1 + instVarName2').
	self
		assert: (class classSide parseTreeForSelector: #bar)
		equals:
			(self
				parseMethod:
					'bar
							^self printString')
]
