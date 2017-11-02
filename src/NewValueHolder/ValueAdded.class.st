"
A ValueAdded is an announcement raised when an item (or a collection of items) was added to CollectionValueHolder.
"
Class {
	#name : #ValueAdded,
	#superclass : #Announcement,
	#instVars : [
		'newValue'
	],
	#category : 'NewValueHolder'
}

{ #category : #'instance creation' }
ValueAdded class >> newValue: newValue [
	^ self new
		newValue: newValue;
		yourself
]

{ #category : #accessing }
ValueAdded >> newValue [
	^ newValue
]

{ #category : #accessing }
ValueAdded >> newValue: anObject [
	newValue := anObject
]
