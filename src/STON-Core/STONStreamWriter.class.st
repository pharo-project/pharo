"
STONStreamWriter helps in streaming writing STON representations.
This is an abstract class.
"
Class {
	#name : 'STONStreamWriter',
	#superclass : 'Object',
	#instVars : [
		'writer',
		'first'
	],
	#category : 'STON-Core-Writer',
	#package : 'STON-Core',
	#tag : 'Writer'
}

{ #category : 'instance creation' }
STONStreamWriter class >> on: stonWriter [
	^ self new
		on: stonWriter;
		yourself
]

{ #category : 'initialization' }
STONStreamWriter >> initialize [
	super initialize.
	first := true
]

{ #category : 'initialization' }
STONStreamWriter >> on: stonWriter [
	writer := stonWriter
]
