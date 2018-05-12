"
I'm a simple test ressource for test purposes
"
Class {
	#name : #SimpleTestResourceA,
	#superclass : #SimpleTestResource,
	#category : #'SUnit-Tests-Resources'
}

{ #category : #accessing }
SimpleTestResourceA class >> resources [
	^Array with: SimpleTestResourceA1 with: SimpleTestResourceA2
]
