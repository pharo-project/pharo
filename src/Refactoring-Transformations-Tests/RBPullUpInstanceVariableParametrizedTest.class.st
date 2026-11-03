Class {
	#name : 'RBPullUpInstanceVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBPullUpInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RePullUpInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPullUpVariableTransformation .
					  #constructor -> #instanceVariable:class: };
		yourself
]

{ #category : 'running' }
RBPullUpInstanceVariableParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBLintRuleTestData . RBBasicLintRuleTestData . RBFooLintRuleTestData }
]

{ #category : 'failure tests' }
RBPullUpInstanceVariableParametrizedTest >> testFailurePullUpVariableNotDefined [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'notDefinedVariable'. #RBLintRuleTestData })
]

{ #category : 'tests' }
RBPullUpInstanceVariableParametrizedTest >> testPullUpInstVar [
	| refactoring root firstChild secondChild |
	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #PullUpInstanceVariableRoot;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #PullUpInstanceVariableRoot;
				name: #PullUpInstanceVariableFirstChild;
				slots: { #result };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #PullUpInstanceVariableRoot;
				name: #PullUpInstanceVariableSecondChild;
				slots: { #result };
				package: #'Refactory-Test data' ].
	refactoring := self createRefactoringWithModel: model andArguments:
			{ 'result'. #PullUpInstanceVariableRoot }.
	self executeRefactoring: refactoring.
	root := refactoring model classNamed: #PullUpInstanceVariableRoot.
	firstChild := refactoring model classNamed: #PullUpInstanceVariableFirstChild.
	secondChild := refactoring model classNamed: #PullUpInstanceVariableSecondChild.
	self assert: (root
		directlyDefinesInstanceVariable: 'result').
	self deny: (firstChild
		directlyDefinesInstanceVariable: 'result').
	self deny: (secondChild
		directlyDefinesInstanceVariable: 'result')
]

{ #category : 'tests' }
RBPullUpInstanceVariableParametrizedTest >> testPullUpInstVarOnMetaclass [
	| refactoring root firstChild secondChild |
	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #PullUpClassSideInstanceVariableRoot;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #PullUpClassSideInstanceVariableRoot;
				name: #PullUpClassSideInstanceVariableFirstChild;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #PullUpClassSideInstanceVariableRoot;
				name: #PullUpClassSideInstanceVariableSecondChild;
				package: #'Refactory-Test data' ].
	root := model classNamed: #PullUpClassSideInstanceVariableRoot.
	firstChild := model classNamed: #PullUpClassSideInstanceVariableFirstChild.
	secondChild := model classNamed: #PullUpClassSideInstanceVariableSecondChild.
	firstChild classSide addInstanceVariable: #result.
	secondChild classSide addInstanceVariable: #result.

	refactoring := self createRefactoringWithModel: model andArguments:
			{ #result. root classSide }.
	self executeRefactoring: refactoring.

	self assert: (root classSide directlyDefinesInstanceVariable: #result).
	self deny: (firstChild classSide directlyDefinesInstanceVariable: #result).
	self deny: (secondChild classSide directlyDefinesInstanceVariable: #result)
]
