"
A ValueRemoved  is an announcement raised when an item (or a collection of items) was removed from CollectionValueHolder.
"
Class {
	#name : 'ValueRemoved',
	#superclass : 'Announcement',
	#instVars : [
		'oldValue'
	],
	#category : 'NewValueHolder-Core-Announcements',
	#package : 'NewValueHolder-Core',
	#tag : 'Announcements'
}

{ #category : 'instance creation' }
ValueRemoved class >> oldValue: oldValue [
	^ self new
		oldValue: oldValue;
		yourself
]

{ #category : 'accessing' }
ValueRemoved >> oldValue [
	^ oldValue
]

{ #category : 'accessing' }
ValueRemoved >> oldValue: anObject [
	oldValue := anObject
]
