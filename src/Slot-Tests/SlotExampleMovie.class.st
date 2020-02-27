"
A SlotExampleMovie is an example class to demonstrate the workings of relation slots.
"
Class {
	#name : #SlotExampleMovie,
	#superclass : #Object,
	#instVars : [
		'#name',
		'#director => ToOneRelationSlot inverse: #directedMovies inClass: #SlotExamplePerson',
		'#actors => ToManyRelationSlot inverse: #actedInMovies inClass: #SlotExamplePerson'
	],
	#category : #'Slot-Tests-Examples - Associations'
}

{ #category : #'instance creation' }
SlotExampleMovie class >> named: aString [
	^ self new
		name: aString;
		yourself
]

{ #category : #accessing }
SlotExampleMovie >> actors [
	^ actors
]

{ #category : #accessing }
SlotExampleMovie >> actors: anObject [
	actors := anObject
]

{ #category : #'adding/removing' }
SlotExampleMovie >> addActor: aPerson [

	^actors add: aPerson
]

{ #category : #accessing }
SlotExampleMovie >> director [
	^ director
]

{ #category : #accessing }
SlotExampleMovie >> director: aPerson [

	director := aPerson
]

{ #category : #initialization }
SlotExampleMovie >> initialize [
	super initialize.
	self class initializeSlots: self
]

{ #category : #accessing }
SlotExampleMovie >> name [
	^ name
]

{ #category : #accessing }
SlotExampleMovie >> name: anObject [
	name := anObject
]

{ #category : #'adding/removing' }
SlotExampleMovie >> removeActor: aPerson [

	^actors remove: aPerson
]
