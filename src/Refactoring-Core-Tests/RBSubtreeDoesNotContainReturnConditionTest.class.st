Class {
	#name : 'RBSubtreeDoesNotContainReturnConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
RBSubtreeDoesNotContainReturnConditionTest >> testFailureWhenSubtreeContainsReturnExpectFalse [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '^ ''Just for test purpose'''.
	
	precondition := ReSubtreeDoesNotContainReturnCondition new subtree: subtree.
	
	self deny: precondition check.
	^ 'Just for test purpose' "maybe this is a bad thing to do in a test, I can change it if needed"
]

{ #category : 'tests' }
RBSubtreeDoesNotContainReturnConditionTest >> testSubtreeDoesNotContainReturnExpectTrue [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: 'precondition check'.
	
	precondition := ReSubtreeDoesNotContainReturnCondition new subtree: subtree.
	
	self assert: precondition check
]
