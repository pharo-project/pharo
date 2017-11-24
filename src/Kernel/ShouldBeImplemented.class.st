"
I am ShouldBeImplemented, an exception signaled when some method should have been implemented but was not.

This is more like a placeholder during development.
"
Class {
	#name : #ShouldBeImplemented,
	#superclass : #SelectorException,
	#category : #'Kernel-Exceptions'
}

{ #category : #printing }
ShouldBeImplemented >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self selector.
		stream << ' should have been implemented in '.
		stream print: self signaler class]
]
