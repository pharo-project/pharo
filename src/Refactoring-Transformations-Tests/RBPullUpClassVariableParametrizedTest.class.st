Class {
	#name : 'RBPullUpClassVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBPullUpClassVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RePullUpSharedVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPullUpVariableTransformation .
					  #constructor -> #classVariable:class: };
		yourself
]

{ #category : 'running' }
RBPullUpClassVariableParametrizedTest >> setUp [

	| classesInModel |
	super setUp.
	classesInModel := { RBLintRuleTestData } asOrderedCollection
		                  addAll: RBLintRuleTestData allSubclasses;
		                  yourself.
	model := self modelOnClasses: classesInModel
]

{ #category : 'failure tests' }
RBPullUpClassVariableParametrizedTest >> testFailureMetaClassFailure [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { #RecursiveSelfRule. #'RBLintRuleTestData class' })
]

{ #category : 'failure tests' }
RBPullUpClassVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail:
		(self createRefactoringWithModel: model andArguments: { #Foo. #RBLintRuleTestData })
]

{ #category : 'tests' }
RBPullUpClassVariableParametrizedTest >> testPullUpClassVariable [
	| refactoring root firstChild secondChild |
	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #PullUpClassVariableRoot;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #PullUpClassVariableRoot;
				name: #PullUpClassVariableFirstChild;
				sharedVariables: { #Foo };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #PullUpClassVariableRoot;
				name: #PullUpClassVariableSecondChild;
				sharedVariables: { #Foo };
				package: #'Refactory-Test data' ].
	refactoring := self createRefactoringWithModel: model andArguments:
		{ #Foo . #PullUpClassVariableRoot }.
	self executeRefactoring: refactoring.
	root := refactoring model classNamed: #PullUpClassVariableRoot.
	firstChild := refactoring model classNamed: #PullUpClassVariableFirstChild.
	secondChild := refactoring model classNamed: #PullUpClassVariableSecondChild.
	self assert: (root directlyDefinesClassVariable: #Foo).
	self deny: (firstChild directlyDefinesClassVariable: #Foo).
	self deny: (secondChild directlyDefinesClassVariable: #Foo)
]
