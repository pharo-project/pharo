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

	| defaultDescription messageNotUndestoodDescription |
	defaultDescription := ''.
	messageNotUndestoodDescription := ''.
	[ Object aMessageThatDoesntExistsIntentiallyRequested ]
		on: MessageNotUnderstood
		do: [ :ex | 
			defaultDescription := ex defaultDescription.
			messageNotUndestoodDescription := ex description ].
	self deny: defaultDescription identicalTo: messageNotUndestoodDescription
]
