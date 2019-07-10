Class {
	#name : #SpecMillerActivation,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#category : #'Spec-Core-Miller'
}

{ #category : #'instance creation' }
SpecMillerActivation class >> on: anInteger [ 
	
	^ self new
		value: anInteger;
		yourself
]

{ #category : #accessing }
SpecMillerActivation >> selectedItem [
	
	^ value
]

{ #category : #accessing }
SpecMillerActivation >> value: anInteger [ 
	value := anInteger
]
