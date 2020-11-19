"
This exception is provided to support Object>>doesNotUnderstand:.
"
Class {
	#name : #MessageNotUnderstood,
	#superclass : #Error,
	#instVars : [
		'message',
		'receiver',
		'reachedDefaultHandler'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #accessing }
MessageNotUnderstood >> defaultAction [

	reachedDefaultHandler := true.
	super defaultAction.
]

{ #category : #accessing }
MessageNotUnderstood >> description [

	"Returns a textual description of the exception (based on the message instance variable). If message is nil, it returns the defaultDescription instead."

	message ifNil: [ ^ self defaultDescription ].
	message lookupClass == UndefinedObject ifTrue: [ 
		^ message selector printString , ' was sent to nil' ].

	^ 'Instance of ' , message lookupClass printString
	  , ' did not understand ' , message selector printString
]

{ #category : #initialization }
MessageNotUnderstood >> initialize [

	super initialize.
	reachedDefaultHandler := false	
]

{ #category : #private }
MessageNotUnderstood >> isResumable [
	"Determine whether an exception is resumable."

	^true
]

{ #category : #accessing }
MessageNotUnderstood >> message [
	"Answer the selector and arguments of the message that failed."

	^message
]

{ #category : #accessing }
MessageNotUnderstood >> message: aMessage [

	message := aMessage
]

{ #category : #accessing }
MessageNotUnderstood >> messageText [
	"Return an exception's message text."
	
	^ messageText 
			ifNil:
				[message 
					ifNil: [super messageText]
					ifNotNil: [
						message lookupClass == UndefinedObject
							ifTrue: ['receiver of "{1}" is nil' format: {message selector asString}]
							ifFalse: [message lookupClass printString, '>> #', message selector asString]]]
]

{ #category : #accessing }
MessageNotUnderstood >> reachedDefaultHandler [
	^reachedDefaultHandler
]

{ #category : #accessing }
MessageNotUnderstood >> receiver [
	"Answer the receiver that did not understand the message"

	^ receiver
]

{ #category : #accessing }
MessageNotUnderstood >> receiver: obj [

	receiver := obj
]
