Class {
	#name : 'ReClassesHaveNoReferencesTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassesHaveNoReferencesTest >> testClassHasNoReferences [

	| unreferenced cond |
	unreferenced := self model classNamed: #MySubAccessingSuperclassState.

	cond := ReClassesHaveNoReferencesCondition new
		        model: self model;
		        classes: { unreferenced }.

	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { unreferenced }
]

{ #category : 'tests' }
ReClassesHaveNoReferencesTest >> testClassHasReferences [

	| referenced cond violator |
	referenced := self model classNamed: #MyClassARoot.
	cond := ReClassesHaveNoReferencesCondition new
		        model: self model;
		        classes: { referenced }.
	self deny: cond check.

	self assert: cond violators size equals: 2.

	violator := cond violators first.
	self assert: violator selector equals: #accessingSuperclass.
	self assert: violator methodClass name equals: #MySubAccessingSuperclassState
]

{ #category : 'tests' }
ReClassesHaveNoReferencesTest >> testSomeClassesHaveReferences [

	| referenced unreferenced cond |
	referenced := self model classNamed: #MyClassARoot.
	unreferenced := self model classNamed: #MyClassB.

	cond := ReClassesHaveNoReferencesCondition new
		        model: self model;
		        classes: { referenced . unreferenced }.

	self deny: cond check.
	self assert: cond nonViolators equals: { referenced . unreferenced }
]
