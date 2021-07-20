"
A VariableNotDeclaredTest is a test class for testing the behavior of VariableNotDeclared
"
Class {
	#name : #VariableNotDeclaredTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Exceptions'
}

{ #category : #tests }
VariableNotDeclaredTest >> testDescription [

	"Tests that the description of the VariableNotDeclaredTest is not the default of Exception"

	| exception defaultDescription variableNotDeclaredDescription |
	defaultDescription := ''.
	variableNotDeclaredDescription := ''.
	exception:= MyVariableNotDeclaredDummyClassForTest new. 
	defaultDescription := exception defaultDescription .
	variableNotDeclaredDescription := exception description .
	self deny: defaultDescription equals: variableNotDeclaredDescription 
]
