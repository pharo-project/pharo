"
A RBTemporariesNotReadBeforeWrittenConditionTest is a test class for testing the behavior of RBTemporariesNotReadBeforeWrittenCondition
"
Class {
	#name : 'RBTemporariesNotReadBeforeWrittenConditionTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'utilities' }
RBTemporariesNotReadBeforeWrittenConditionTest >> modelOnClasses: aCollection [

	^ RBNamespace onEnvironment:
		  (RBClassEnvironment classes: aCollection)
]

{ #category : 'tests' }
RBTemporariesNotReadBeforeWrittenConditionTest >> testNoTemporariesInSubtree [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: 'precondition check'.
	
	precondition := RBTemporariesNotReadBeforeWrittenCondition new subtree: subtree temporaries: #().
	
	self assert: precondition check
]

{ #category : 'tests' }
RBTemporariesNotReadBeforeWrittenConditionTest >> testSubtreeIsNilExpectFalse [

	| precondition |.
	
	"model is read before written in the code snippet"
	precondition := RBTemporariesNotReadBeforeWrittenCondition new subtree: nil temporaries: #(#model).
	
	self deny: precondition check
]

{ #category : 'tests' }
RBTemporariesNotReadBeforeWrittenConditionTest >> testTemporaryIsNotReadBeforeWritten [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.'.
	
	precondition := RBTemporariesNotReadBeforeWrittenCondition new subtree: subtree temporaries: #(#subtree  #model  #class #parseTree).
	
	self assert: precondition check
]

{ #category : 'tests' }
RBTemporariesNotReadBeforeWrittenConditionTest >> testTemporaryIsReadBeforeWritten [

	| precondition model class parseTree subtree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
	parseTree := class parseTreeForSelector: self selector.
	subtree := parseTree extractSubtreeWith: '
	class := model classObjectFor: self class.'.
	
	"model is read before written in the code snippet"
	precondition := RBTemporariesNotReadBeforeWrittenCondition new subtree: subtree temporaries: #(#model #class).
	
	self deny: precondition check
]
