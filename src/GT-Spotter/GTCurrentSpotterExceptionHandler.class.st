Class {
	#name : #GTCurrentSpotterExceptionHandler,
	#superclass : #Notification,
	#category : #'GT-Spotter-Exceptions'
}

{ #category : #'accessing-defaults' }
GTCurrentSpotterExceptionHandler class >> defaultValue [
	^ nil
]

{ #category : #evaluating }
GTCurrentSpotterExceptionHandler class >> use: anObject during: aBlock [
	
	^ aBlock on: self do: [ :notification | notification resume: anObject ]
]

{ #category : #evaluating }
GTCurrentSpotterExceptionHandler class >> value [
	^ self signal
]

{ #category : #'accessing-defaults' }
GTCurrentSpotterExceptionHandler >> defaultAction [
	^ self class defaultValue
]
