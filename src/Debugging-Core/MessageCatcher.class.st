"
Any message sent to me is returned as a Message object.

""Message catcher"" creates an instance of me.

"
Class {
	#name : 'MessageCatcher',
	#superclass : 'ProtoObject',
	#instVars : [
		'accumulator'
	],
	#category : 'Debugging-Core',
	#package : 'Debugging-Core'
}

{ #category : 'reflective operations' }
MessageCatcher >> doesNotUnderstand: aMessage [

	accumulator ifNotNil: [accumulator add: aMessage].
	^ aMessage
]

{ #category : 'private' }
MessageCatcher >> privAccumulator [

	^ accumulator
]

{ #category : 'private' }
MessageCatcher >> privAccumulator: collection [

	accumulator := collection
]
