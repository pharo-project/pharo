"
A ValueChanged is an announcement raised when a value of a NewValueHolder is changed
"
Class {
	#name : #ValueChanged,
	#superclass : #Announcement,
	#instVars : [
		'newValue',
		'oldValue'
	],
	#category : #'System-Model'
}

{ #category : #'instance creation' }
ValueChanged class >> newValue: newContents [

	^ self new
		newValue: newContents;
		yourself
]

{ #category : #'instance creation' }
ValueChanged class >> oldValue: oldContent newValue: newValue [

	^ self new
		newValue: newValue;
		oldValue: oldContent;
		yourself
]

{ #category : #accessing }
ValueChanged >> newValue [

	^ newValue
]

{ #category : #accessing }
ValueChanged >> newValue: anObject [
	
	newValue := anObject
]

{ #category : #accessing }
ValueChanged >> oldValue [

	^ oldValue
]

{ #category : #accessing }
ValueChanged >> oldValue: anObject [
	
	oldValue := anObject
]
