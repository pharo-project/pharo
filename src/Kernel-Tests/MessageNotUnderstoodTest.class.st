"
A MessageNotUnderstoodTest is a test class for testing the behavior of MessageNotUnderstood
"
Class {
	#name : 'MessageNotUnderstoodTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Exceptions',
	#package : 'Kernel-Tests',
	#tag : 'Exceptions'
}

{ #category : 'tests' }
MessageNotUnderstoodTest >> testDescription [
	
	| defaultDescription messageNotUndestoodDescription |
	<ignoreNotImplementedSelectors: #(aMessageThatDoesntExistsIntentiallyRequested)>
	defaultDescription := ''.
	messageNotUndestoodDescription := ''.
	"Note that in the following line, the IDE complaints that the method doesn't exists. This is intentional."
	[ Object aMessageThatDoesntExistsIntentiallyRequested ]
		on: MessageNotUnderstood
		do: [ :ex |
			defaultDescription := ex defaultDescription.
			messageNotUndestoodDescription := ex description ].
	self deny: defaultDescription equals: messageNotUndestoodDescription
]

{ #category : 'tests' }
MessageNotUnderstoodTest >> testMessageText [

	| messageText |
	[ 1 a ]
		on: Error
		do: [ :e | messageText := e messageText ].
	self
		assert: messageText
		equals: 'Message not understood: SmallInteger >> #a'
]
