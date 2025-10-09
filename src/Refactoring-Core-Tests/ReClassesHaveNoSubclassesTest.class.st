Class {
	#name : 'ReClassesHaveNoSubclassesTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassesHaveNoSubclassesTest >> testClassHasNoSubclasses [

	| nosubs cond |
	nosubs := self model classNamed: #MySubAccessingSuperclassState.

	cond := ReClassesHaveNoSubclassesCondition new
		        classes: { nosubs }.

	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { nosubs }
]

{ #category : 'tests' }
ReClassesHaveNoSubclassesTest >> testClassHasSubclasses [

	| hassubs cond |
	hassubs := self model classNamed: #MyClassARoot.

	cond := ReClassesHaveNoSubclassesCondition new
		        classes: { hassubs }.

	self deny: cond check.
	self assert: cond violators equals: { hassubs }.
	self assert: cond nonViolators isEmpty
]

{ #category : 'tests' }
ReClassesHaveNoSubclassesTest >> testSomeClassesHaveSubclasses [

	| hassubs nosubs cond |
	hassubs := self model classNamed: #MyClassARoot.
	nosubs := self model classNamed: #MySubAccessingSuperclassState.

	cond := ReClassesHaveNoSubclassesCondition new
		        classes: { hassubs . nosubs }.

	self deny: cond check.
	self assert: cond violators equals: { hassubs }.
	self assert: cond nonViolators equals: { nosubs }. 
]
