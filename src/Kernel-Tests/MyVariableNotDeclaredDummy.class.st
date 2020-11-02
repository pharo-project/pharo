"
For the Class part:  ""I represent a VariableNotDeclared with overriden methods for unit tests"".

For the Responsibility part: Used for testing the base class.
"
Class {
	#name : #MyVariableNotDeclaredDummy,
	#superclass : #VariableNotDeclared,
	#category : #'Kernel-Tests-Exceptions'
}

{ #category : #accessing }
MyVariableNotDeclaredDummy >> messageBasedDescription [ 
	"Overrides the method for testing"	
	^ 'messageBasedDescription'
]
