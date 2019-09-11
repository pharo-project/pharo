"
A SlotExamplePerson is an example class to demonstrate the workings of relation slots.
"
Class {
	#name : #SlotExamplePerson,
	#superclass : #Object,
	#instVars : [
		'#name',
		'#directedMovies => ToManyRelationSlot inverse: #director inClass: #SlotExampleMovie',
		'#actedInMovies => ToManyRelationSlot inverse: #actors inClass: #SlotExampleMovie'
	],
	#category : #'Slot-Tests-Examples - Associations'
}

{ #category : #'instance creation' }
SlotExamplePerson class >> named: aString [
	^ self new
		name: aString;
		yourself
]

{ #category : #accessing }
SlotExamplePerson >> actedInMovies [
	^ actedInMovies
]

{ #category : #accessing }
SlotExamplePerson >> actedInMovies: anObject [
	actedInMovies := anObject
]

{ #category : #accessing }
SlotExamplePerson >> directedMovies [
	^ directedMovies
]

{ #category : #accessing }
SlotExamplePerson >> directedMovies: anObject [
	directedMovies := anObject
]

{ #category : #initialization }
SlotExamplePerson >> initialize [
	super initialize.
	self class initializeSlots: self
]

{ #category : #accessing }
SlotExamplePerson >> name [
	
	^name
]

{ #category : #accessing }
SlotExamplePerson >> name: anObject [
	name := anObject
]
