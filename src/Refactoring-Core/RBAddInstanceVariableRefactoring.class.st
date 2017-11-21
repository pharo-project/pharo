"
I am a refactoring for adding new instance variables.

My precondition verifies that the variable name is valid, not yet used in the whole hierarchy and not a global name.
"
Class {
	#name : #RBAddInstanceVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#category : #Refactoring-Core-Refactorings
}

{ #category : #preconditions }
RBAddInstanceVariableRefactoring >> preconditions [
	^(RBCondition isValidInstanceVariableName: variableName for: class) 
		& (RBCondition hierarchyOf: class definesVariable: variableName) not 
			& (RBCondition isGlobal: variableName in: self model) not
]

{ #category : #transforming }
RBAddInstanceVariableRefactoring >> transform [
	class addInstanceVariable: variableName
]
