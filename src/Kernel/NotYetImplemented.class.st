"
I am NotYetImplement, an exception signaled when a method is a stub for code that will be implemented in the future.

This is used in incremental development, for example when doing Test First development.
It is similar to ShouldBeImplemented, with a slightly different meaning.
"
Class {
	#name : #NotYetImplemented,
	#superclass : #SelectorException,
	#category : #Kernel-Exceptions
}

{ #category : #printing }
NotYetImplemented >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self selector.
		stream << ' is not yet implemented in '.
		stream print: self signaler class]
]
