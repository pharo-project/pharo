"
When an image is launched, a session starts. When quits, the session ends. In the middle, it could be snapshoted multiple times. My subclasses represent such events.
"
Class {
	#name : #EpSessionEvent,
	#superclass : #EpEvent,
	#instVars : [
		'imageName'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpSessionEvent class >> imageName: anImageName [
	^ self basicNew
		initializeWith: anImageName;
		yourself
]

{ #category : #'instance creation' }
EpSessionEvent class >> new [ 
	^ self imageName: Smalltalk image imagePath
]

{ #category : #accessing }
EpSessionEvent >> imageName [
	^ imageName
]

{ #category : #initialization }
EpSessionEvent >> initializeWith: anImageName [
	self initialize.
	
	imageName := anImageName
]
