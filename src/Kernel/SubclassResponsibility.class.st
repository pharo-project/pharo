"
I am SubclassResponsibility, an exception signaled when an inherited method should have been implemented in a subclass but was not.
"
Class {
	#name : #SubclassResponsibility,
	#superclass : #SelectorException,
	#category : #'Kernel-Exceptions'
}

{ #category : #printing }
SubclassResponsibility >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self signaler class.
		stream << ' had the subclass responsibility to implement '.
		stream print: self selector ]
]
