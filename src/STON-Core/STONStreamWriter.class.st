"
STONStreamWriter helps in streaming writing STON representations.
This is an abstract class.
"
Class {
	#name : #STONStreamWriter,
	#superclass : #Object,
	#instVars : [
		'writer',
		'first'
	],
	#category : 'STON-Core'
}

{ #category : #'instance creation' }
STONStreamWriter class >> on: stonWriter [
	^ self new
		on: stonWriter;
		yourself
]

{ #category : #initialize-release }
STONStreamWriter >> initialize [
	super initialize.
	first := true
]

{ #category : #initialize-release }
STONStreamWriter >> on: stonWriter [
	writer := stonWriter
]
