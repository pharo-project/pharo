Class {
	#name : 'RBMaxOneAssignmentWithReferencesConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
RBMaxOneAssignmentWithReferencesConditionTest >> testFailureMaxOneAssignmentWhenTwoAssignmentsExpectFalse [

	| precondition |
	
	precondition := ReMaxOneAssignmentWithReferencesCondition new assignments: #(class #model).
	
	self deny: precondition check
]

{ #category : 'tests' }
RBMaxOneAssignmentWithReferencesConditionTest >> testFailureWhenAssignmentsIsNilExpectFalse [

	| precondition |
	
	precondition := ReMaxOneAssignmentWithReferencesCondition new assignments: nil.

	self deny: precondition check
]

{ #category : 'tests' }
RBMaxOneAssignmentWithReferencesConditionTest >> testMaxOneAssignmentWhenNoAssignmentsExpectTrue [

	| precondition  |
	
	precondition := ReMaxOneAssignmentWithReferencesCondition new assignments: #().
	
	self assert: precondition check
]

{ #category : 'tests' }
RBMaxOneAssignmentWithReferencesConditionTest >> testMaxOneAssignmentWhenOneAssignmentsExpectTrue [

	| precondition |
	
	precondition := ReMaxOneAssignmentWithReferencesCondition new assignments: #(#model).
	
	self assert: precondition check
]
