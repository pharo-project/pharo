Class {
	#name : 'ReSingleAssignmentConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'tests' }
ReSingleAssignmentConditionTest >> testMultipleAssignment [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree :=  class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '(testVariable := value1 . testVariable := value2)'.
	
	precondition := ReSingleAssignmentCondition new initialize: 'testVariable' parseTree:subtree.
	
	self deny: precondition check
]

{ #category : 'tests' }
ReSingleAssignmentConditionTest >> testSingleAssignment [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree :=  class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '(testVariable := value1)'.
	
	precondition := ReSingleAssignmentCondition new initialize: 'testVariable' parseTree:subtree.
	
	self assert: precondition check
]
