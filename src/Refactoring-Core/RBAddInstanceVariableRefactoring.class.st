"
I am a refactoring for adding new instance variables.

My precondition verifies that the variable name is valid, not yet used in the whole hierarchy and not a global name.
"
Class {
	#name : 'RBAddInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBAddInstanceVariableRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition isValidInstanceVariableName: variableName for: class).
		  (RBCondition isGlobal: variableName in: self model) not.
		  (RBCondition hierarchyOf: class definesVariable: variableName) not }
]

{ #category : 'preconditions' }
RBAddInstanceVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RBAddInstanceVariableRefactoring >> privateTransform [
	class addInstanceVariable: variableName
]
