"
A MessageNotUnderstoodTest is a test class for testing the behavior of MessageNotUnderstood
"
Class {
	#name : #MessageNotUnderstoodTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Exceptions'
}

{ #category : #tests }
MessageNotUnderstoodTest >> testDescription [
   "Tests that the description method produces a message-based text description of the exception, if the message field is not nil, or it uses the default description text instead if message nil"

	| aMessageNotUnderstood defaultDescription descriptionWithMessage |
	aMessageNotUnderstood := MyMessageNotUndestoodDummy new.
	"The default description when message is nil"
	defaultDescription := aMessageNotUnderstood description.
	aMessageNotUnderstood message: 'aNonNilValue'.
		"The message-based description. It is a constant in our dummy class overriden accesor."
	descriptionWithMessage := aMessageNotUnderstood description.
	self assert: defaultDescription ~~ descriptionWithMessage
]
