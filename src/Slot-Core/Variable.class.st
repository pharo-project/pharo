Class {
	#name : #Variable,
	#superclass : #Object,
	#instVars : [
		'name'
	],
	#classVars : [
		'Properties'
	],
	#category : #'Slot-Core-Variables'
}

{ #category : #comparing }
Variable >> = other [
	"Every subclass that adds state must redefine this method"
	self == other
		ifTrue: [ ^ true ].
	^ (self species == other species) 
			and: [ name = other name ]
]

{ #category : #comparing }
Variable >> hash [
	"Every subclass that adds state must redefine this method"
	^ self species hash bitXor: self name hash
]

{ #category : #accessing }
Variable >> name [
	^ name
]

{ #category : #accessing }
Variable >> name: aSymbol [
	name := aSymbol
]
