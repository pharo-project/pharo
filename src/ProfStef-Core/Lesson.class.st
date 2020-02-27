"
A lesson is a simple data object holding a name and an associated text that will be displayed by
the tutorialPlayer. 

"
Class {
	#name : #Lesson,
	#superclass : #Object,
	#instVars : [
		'title',
		'lesson'
	],
	#category : #'ProfStef-Core'
}

{ #category : #'instance creation' }
Lesson class >> title: aStringForTitle lesson: aStringForLesson [
	^ self new title: aStringForTitle; lesson: aStringForLesson; yourself.
]

{ #category : #accessing }
Lesson >> lesson [
	^ lesson ifNil: [lesson := '']
]

{ #category : #accessing }
Lesson >> lesson: aString [
	lesson := aString
]

{ #category : #printing }
Lesson >> printOn: aStream [
	super printOn: aStream.  
	aStream 
		nextPutAll: '(';
		nextPutAll: self title;
		nextPutAll: ')'.    
]

{ #category : #accessing }
Lesson >> title [
	^ title ifNil: [title := '']
]

{ #category : #accessing }
Lesson >> title: aString [
	title := aString
]
