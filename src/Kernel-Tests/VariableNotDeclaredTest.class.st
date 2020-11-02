"
A VariableNotDeclaredTest is a test class for testing the behavior of VariableNotDeclared
"
Class {
	#name : #VariableNotDeclaredTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Exceptions'
}

{ #category : #test }
VariableNotDeclaredTest >> testDescription [
   "Tests that the description method produces a message-based text description of the exception, if the message field is not nil, or it uses the default description text instead if message nil"

	| aVariableNotDeclared defaultDescription descriptionWithMessage |
	aVariableNotDeclared := MyVariableNotDeclaredDummy new.
	"The default description when message is nil"
	defaultDescription := aVariableNotDeclared description.
	aVariableNotDeclared message: 'aNonNilValue'.
		"The message-based description. It is a constant in our dummy class overriden accesor."
	descriptionWithMessage := aVariableNotDeclared description.
	self assert: defaultDescription ~~ descriptionWithMessage
]
