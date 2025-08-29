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
	"Pay attention the test is using the body of this method."
	
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '(precondition not) errorString'.
	
	precondition := ReNotInCascadedMessageCondition new subtree: subtree.
	
	(precondition not) errorString; errorString.
	
	self deny: precondition check
]

{ #category : 'tests' }
RBNotInCascadedMessageConditionTest >> testNotInCascadedMessageExpectTrue [

	| precondition model class parseTree subtree |
	"Pay attention the test is using the method itself as input."
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: 'precondition not'.

	precondition := ReNotInCascadedMessageCondition new subtree: subtree.
	
	(precondition not) errorString; errorString.
	
	self assert: precondition check
]
