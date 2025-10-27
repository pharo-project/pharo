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

	| referenced cond |
	referenced := self model classNamed: #MyClassARoot.

	cond := ReClassesHaveNoReferencesCondition new
		        model: self model;
		        classes: { referenced }.

	self deny: cond check.
	self assert: cond violators equals: { referenced }.
	self assert: cond nonViolators isEmpty 
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
	self assert: cond violators equals: { referenced }.
	self assert: cond nonViolators equals: { unreferenced }
]
