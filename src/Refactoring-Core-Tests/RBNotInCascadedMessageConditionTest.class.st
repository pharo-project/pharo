Class {
	#name : 'RBNotInCascadedMessageConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
RBNotInCascadedMessageConditionTest >> testInCascadedMessageExpectFalse [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '(precondition not) errorBlock'.
	
	precondition := ReNotInCascadedMessageCondition new subtree: subtree.
	
	(precondition not) errorBlock; errorBlock.
	
	self deny: precondition check
]

{ #category : 'tests' }
RBNotInCascadedMessageConditionTest >> testNotInCascadedMessageExpectTrue [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: 'precondition not'.

	precondition := ReNotInCascadedMessageCondition new subtree: subtree.
	
	(precondition not) errorBlock; errorBlock.
	
	self assert: precondition check
]
