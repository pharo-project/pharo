Class {
	#name : 'ReSingleAssignmentConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests - fixture' }
ReSingleAssignmentConditionTest >> multipleAssignmentMethod [

	| testVariable |
	testVariable := 1.
	testVariable := 2.
]

{ #category : 'tests - fixture' }
ReSingleAssignmentConditionTest >> singleAssignmentMethod [

	| testVariable |
	testVariable := 1.
]

{ #category : 'tests' }
ReSingleAssignmentConditionTest >> testMultipleAssignment [

	| precondition model class parseTree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
   parseTree := class parseTreeForSelector: #multipleAssignmentMethod.

   precondition := ReSingleAssignmentCondition new.
	precondition variableName: 'testVariable'; definingNode: parseTree.
    
   self deny: precondition check.
]

{ #category : 'tests' }
ReSingleAssignmentConditionTest >> testSingleAssignment [

	| precondition model class parseTree |
	model := self modelOnClasses: { self class }.
	class := model classObjectFor: self class.
   parseTree := class parseTreeForSelector: #singleAssignmentMethod.

   precondition := ReSingleAssignmentCondition new.
	precondition variableName: 'testVariable'; definingNode: parseTree.
    
   self assert: precondition check.
]
