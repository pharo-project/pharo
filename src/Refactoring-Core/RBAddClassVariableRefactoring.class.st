"
I am a refactoring for adding new class variables.

My precondition verifies that the variable name is valid, not yet used in the whole hierarchy and not a global name.
"
Class {
	#name : #RBAddClassVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#category : #Refactoring-Core-Refactorings
}

{ #category : #preconditions }
RBAddClassVariableRefactoring >> preconditions [
	^(RBCondition isMetaclass: class) not 
		& (RBCondition isValidClassVarName: variableName for: class) 
			& (RBCondition hierarchyOf: class definesVariable: variableName asString) 
					not 
			& (RBCondition isGlobal: variableName in: self model) not
]

{ #category : #transforming }
RBAddClassVariableRefactoring >> transform [
	class addClassVariable: variableName
]
