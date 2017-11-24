"
I represent an object that is kept by a GTEventCollector. I serve only for the testing purpose.
"
Class {
	#name : #GTDummyEvent,
	#superclass : #Object,
	#instVars : [
		'data'
	],
	#category : #'GT-EventRecorder-Tests-Support'
}

{ #category : #comparing }
GTDummyEvent >> = aDummyEvent [
	^ self class = aDummyEvent class and: [ 
		self data = aDummyEvent data ]
]

{ #category : #accessing }
GTDummyEvent >> data [
	^ data
]

{ #category : #accessing }
GTDummyEvent >> data: anObject [
	data := anObject
]

{ #category : #comparing }
GTDummyEvent >> hash [
	^ self class hash bitXor: self data
]
