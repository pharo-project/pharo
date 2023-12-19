Class {
	#name : 'RBMaxOneAssignmentWithReferencesConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'tests' }
RBMaxOneAssignmentWithReferencesConditionTest >> testFailureMaxOneAssignmentWhenTwoAssignmentsExpectFalse [

	| precondition |
	
	precondition := RBMaxOneAssignmentWithReferencesCondition new assignments: #(class #model).
	
	self deny: precondition check
]

{ #category : 'tests' }
RBMaxOneAssignmentWithReferencesConditionTest >> testFailureWhenAssignmentsIsNilExpectFalse [

	| precondition |
	
	precondition := RBMaxOneAssignmentWithReferencesCondition new assignments: nil.

	self deny: precondition check
]

{ #category : 'tests' }
RBMaxOneAssignmentWithReferencesConditionTest >> testMaxOneAssignmentWhenNoAssignmentsExpectTrue [

	| precondition  |
	
	precondition := RBMaxOneAssignmentWithReferencesCondition new assignments: #().
	
	self assert: precondition check
]

{ #category : 'tests' }
RBMaxOneAssignmentWithReferencesConditionTest >> testMaxOneAssignmentWhenOneAssignmentsExpectTrue [

	| precondition |
	
	precondition := RBMaxOneAssignmentWithReferencesCondition new assignments: #(#model).
	
	self assert: precondition check
]
