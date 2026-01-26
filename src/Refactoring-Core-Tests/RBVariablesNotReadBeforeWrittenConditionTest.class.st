"
A RBTemporariesNotReadBeforeWrittenConditionTest is a test class for testing the behavior of RBTemporariesNotReadBeforeWrittenCondition
"
Class {
	#name : 'RBVariablesNotReadBeforeWrittenConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
RBVariablesNotReadBeforeWrittenConditionTest >> testNoVariablesInSubtree [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: 'precondition check'.
	
	precondition := ReVariablesNotReadBeforeWrittenCondition new subtree: subtree; variables: #().
	
	self assert: precondition check
]

{ #category : 'tests' }
RBVariablesNotReadBeforeWrittenConditionTest >> testSubtreeIsNilExpectFalse [

	| precondition |.
	
	"model is read before written in the code snippet"
	precondition := ReVariablesNotReadBeforeWrittenCondition new subtree: nil; variables: #(#model).
	
	self deny: precondition check
]

{ #category : 'tests' }
RBVariablesNotReadBeforeWrittenConditionTest >> testVariableIsNotReadBeforeWritten [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.'.
	
	precondition := ReVariablesNotReadBeforeWrittenCondition new subtree: subtree; variables: #(#subtree  #model  #class #parseTree).
	
	self assert: precondition check
]

{ #category : 'tests' }
RBVariablesNotReadBeforeWrittenConditionTest >> testVariableIsReadBeforeWritten [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '
	class := model classObjectFor: self class.'.
	
	"model is read before written in the code snippet"
	precondition := ReVariablesNotReadBeforeWrittenCondition new subtree: subtree; variables: #(#model #class).
	
	self deny: precondition check
]
