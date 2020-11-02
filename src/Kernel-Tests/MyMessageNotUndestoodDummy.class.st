"
For the Class part:  ""I represent a MessageNotUnderstood with overriden methods for unit tests"".

For the Responsibility part: Used for testing the base class.

"
Class {
	#name : #MyMessageNotUndestoodDummy,
	#superclass : #MessageNotUnderstood,
	#category : #'Kernel-Tests-Exceptions'
}

{ #category : #accessing }
MyMessageNotUndestoodDummy >> messageBasedDescription [ 
	"Overrides the method for testing"	
	^ 'messageBasedDescription'
]
