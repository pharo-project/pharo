"
A GLMLoggedObject is a custom announcer that logs all events that it announces.

Instance Variables
	logger:		<Object>

logger
	- xxxxx

"
Class {
	#name : #GLMLoggedObject,
	#superclass : #GLMAnnouncer,
	#instVars : [
		'logger'
	],
	#classInstVars : [
		'logger'
	],
	#category : #'Glamour-Core'
}

{ #category : #'class initialization' }
GLMLoggedObject class >> initialize [ 
	logger := GLMLogger instance
]

{ #category : #accessing }
GLMLoggedObject class >> logger [
	^ logger
]

{ #category : #accessing }
GLMLoggedObject class >> logger: aLogger [
	logger := aLogger
]

{ #category : #announce }
GLMLoggedObject >> announce: anAnnouncement [
	self logger logAnnouncement: anAnnouncement from: self. 
	super announce: anAnnouncement
]

{ #category : #initialization }
GLMLoggedObject >> initialize [ 
	super initialize.
	logger := GLMLogger instance 
]

{ #category : #accessing }
GLMLoggedObject >> logger [
	^ logger
]

{ #category : #accessing }
GLMLoggedObject >> logger: anObject [
	logger := anObject
]

{ #category : #copying }
GLMLoggedObject >> postCopy [ 
	super postCopy.
	self resetAnnouncer
]
