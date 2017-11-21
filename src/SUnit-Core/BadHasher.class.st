"
I am an object that doesn't always hash correctly.  I am used for testing the HashTester.
"
Class {
	#name : #BadHasher,
	#superclass : #Object,
	#classInstVars : [
		'lastRandomIndex'
	],
	#category : #SUnit-Core-Utilities
}

{ #category : #accessing }
BadHasher class >> randomValue [

	"For every call return different value. Not really random but we do not want to create
	dependency on random number generators."

	| randomNumbers |
	
	randomNumbers := #(2 2 1 2 2 1 2 2 2 2 1 2 1 2 2 2 2 2 1 2). 

	lastRandomIndex ifNil: [ lastRandomIndex := randomNumbers size ].
	
	lastRandomIndex := lastRandomIndex + 1.	
	lastRandomIndex > randomNumbers size 
		ifTrue: [ lastRandomIndex := 1 ].
	
	^ randomNumbers at: lastRandomIndex.
]

{ #category : #comparing }
BadHasher >> hash [
	"answer with a different hash some of the time"
	self class randomValue
]
