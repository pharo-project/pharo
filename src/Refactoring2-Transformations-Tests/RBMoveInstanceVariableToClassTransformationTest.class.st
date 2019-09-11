Class {
	#name : #RBMoveInstanceVariableToClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBMoveInstanceVariableToClassTransformationTest >> testRefactoring [

	| refactoring oldClass newClass |
	refactoring := (RBMoveInstanceVariableToClassTransformation
						model: model
						variable: 'methodBlock'
						fromClass: #RBBasicLintRuleTestData
						toClass: #RBFooLintRuleTestData)
						asRefactoring.
				
	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self assert: (oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self deny: (newClass directlyDefinesInstanceVariable: 'methodBlock').
	
	[ refactoring transform ] on: RBRefactoringError do: [ :e | e resume ].
	self assert: refactoring model changes changes size equals: 2.

	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self deny: (oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self assert: (newClass directlyDefinesInstanceVariable: 'methodBlock').
	
]

{ #category : #testing }
RBMoveInstanceVariableToClassTransformationTest >> testTransform [

	| transformation oldClass newClass |
	model := RBNamespace2 new.
	model defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
				
	transformation := RBMoveInstanceVariableToClassTransformation
							model: model
							variable: 'asdf'
							fromClass: #FOOBAR
							toClass: self changeMock name.
	
	oldClass := transformation model classNamed: #FOOBAR.
	newClass := transformation model classNamed: self changeMock name asSymbol.
	self assert: (oldClass directlyDefinesInstanceVariable: 'asdf').
	self deny: (newClass directlyDefinesInstanceVariable: 'asdf').
	
	transformation transform.
	self assert: transformation model changes changes size equals: 3.
	
	oldClass := transformation model classNamed: #FOOBAR.
	newClass := transformation model classNamed: self changeMock name asSymbol.
	self deny: (oldClass directlyDefinesInstanceVariable: 'asdf').
	self assert: (newClass directlyDefinesInstanceVariable: 'asdf').
	
]

{ #category : #testing }
RBMoveInstanceVariableToClassTransformationTest >> testVariableAlreadyExists [

	self shouldFail: (RBMoveInstanceVariableToClassTransformation
							model: model
							variable: 'result'
							fromClass: #RBFooLintRuleTestData
							toClass: #RBBasicLintRuleTestData)
							asRefactoring
]

{ #category : #testing }
RBMoveInstanceVariableToClassTransformationTest >> testVariableDoesNotExistInOldClass [

	self shouldFail: (RBMoveInstanceVariableToClassTransformation
							model: model
							variable: 'abc'
							fromClass: #RBFooLintRuleTestData
							toClass: #RBBasicLintRuleTestData)
							asRefactoring
]
