"
I am SizeMismatch, an exception indicating that an operation was attempted
on two or more collections that don't match in size. 
"
Class {
	#name : 'SizeMismatch',
	#superclass : 'Error',
	#category : 'Collections-Abstract-Errors',
	#package : 'Collections-Abstract',
	#tag : 'Errors'
}

{ #category : 'accessing' }
SizeMismatch >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"

	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : 'printing' }
SizeMismatch >> standardMessageText [
	"Generate a standard textual description"

	^ 'Collections sizes do not match'
]
