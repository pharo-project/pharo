"
Block and Context cases for CannotReturn need to signal a dedicated exception.
(see subclasses and the related testcases)
"
Class {
	#name : #CannotReturn,
	#superclass : #Error,
	#instVars : [
		'result'
	],
	#category : #Kernel-Exceptions
}

{ #category : #testing }
CannotReturn >> isResumable [

	^true
]

{ #category : #accessing }
CannotReturn >> result [

	^result
]

{ #category : #accessing }
CannotReturn >> result: r [

	result := r
]
