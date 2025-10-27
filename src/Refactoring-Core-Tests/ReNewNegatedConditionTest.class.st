Class {
	#name : 'ReNewNegatedConditionTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReNewNegatedConditionTest >> model [

	^ model ifNil: [
			  model := RBNamespace onEnvironment: (RBClassEnvironment classes: {
						            Number.
						            Object }) ]
]

{ #category : 'tests' }
ReNewNegatedConditionTest >> testNegatedFalseConditionViolators [

	| cls notcond condition |
	cls := self model classNamed: #Object.
	condition := ReClassesAreAbstractCondition new classes: { cls }.
	notcond := condition not.

	self
		deny: condition check;
		assert: notcond check;
		assert: condition violators equals: { cls };
		assert: notcond violators isEmpty ;
		assert: condition nonViolators isEmpty;
		assert: notcond nonViolators equals: { cls }
]

{ #category : 'tests' }
ReNewNegatedConditionTest >> testNegatedTrueConditionViolators [

	| cls notcond condition |
	cls := self model classNamed: #Number.
	condition := ReClassesAreAbstractCondition new classes: { cls }.
	notcond := condition not.

	self
		assert: condition check;
		deny: notcond check;
		assert: condition violators isEmpty;
		assert: notcond violators equals: { cls };
		assert: condition nonViolators equals: { cls };
		assert: notcond nonViolators isEmpty
]
