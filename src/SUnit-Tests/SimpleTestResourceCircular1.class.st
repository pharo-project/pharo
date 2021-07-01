"
I'm a simple test resource for test purposes
"
Class {
	#name : #SimpleTestResourceCircular1,
	#superclass : #SimpleTestResource,
	#category : #'SUnit-Tests-Resources'
}

{ #category : #accessing }
SimpleTestResourceCircular1 class >> resources [
	"Circular prereq:  C wants C1 which wants C."
	
	^Array with: SimpleTestResourceCircular
]
