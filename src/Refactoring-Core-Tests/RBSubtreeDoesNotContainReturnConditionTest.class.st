Class {
	#name : 'RBSubtreeDoesNotContainReturnConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'tests' }
RBSubtreeDoesNotContainReturnConditionTest >> testFailureWhenSubtreeContainsReturnExpectFalse [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '^ ''Just for test purpose'''.
	
	precondition := RBSubtreeDoesNotContainReturnCondition new subtree: subtree.
	
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
	
	precondition := RBSubtreeDoesNotContainReturnCondition new subtree: subtree.
	
	self assert: precondition check
]
