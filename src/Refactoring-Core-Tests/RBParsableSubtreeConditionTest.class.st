Class {
	#name : 'RBParsableSubtreeConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'tests' }
RBParsableSubtreeConditionTest >> testParsableSubtreeExpectTrue [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: 'precondition check'.
	
	precondition := RBParsableSubtreeCondition new subtree: subtree.
	
	self assert: precondition check
]

{ #category : 'tests' }
RBParsableSubtreeConditionTest >> testSubtreeIsNilExpectFalse [

	| precondition |.
	
	"model is read before written in the code snippet"
	precondition := RBParsableSubtreeCondition new subtree: nil.
	
	self deny: precondition check
]
