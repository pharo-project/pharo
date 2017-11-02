"
I am an object that doesn't always report #= correctly.  Used for testing the EqualityTester.
"
Class {
	#name : #BadEqualer,
	#superclass : #Object,
	#category : 'SUnit-Core-Utilities'
}

{ #category : #comparing }
BadEqualer >> = other [ 
	self class = other class
		ifFalse: [^ false].
	^ 100 atRandom < 30 
]
