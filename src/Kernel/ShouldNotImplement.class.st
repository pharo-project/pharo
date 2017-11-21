"
I am ShouldNotImplement, an exception signaled when a method was implemented but should not have been.

This can happen when an implementation is inherited.
"
Class {
	#name : #ShouldNotImplement,
	#superclass : #SelectorException,
	#category : #Kernel-Exceptions
}

{ #category : #printing }
ShouldNotImplement >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self selector.
		stream << ' should not have been implemented in '.
		stream print: self signaler class]
]
