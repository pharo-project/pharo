"
I am a MOCK equivalent for GlobalIdentifierPersistenceChecker, userful for testing behavior.
"
Class {
	#name : #GlobalIdentifierPersistenceMockChecker,
	#superclass : #Object,
	#instVars : [
		'answer'
	],
	#category : #'System-Identification-Tests'
}

{ #category : #accessing }
GlobalIdentifierPersistenceMockChecker >> answer [
	^ answer ifNil: [ answer := false ]
]

{ #category : #accessing }
GlobalIdentifierPersistenceMockChecker >> answer: anObject [
	answer := anObject
]

{ #category : #testing }
GlobalIdentifierPersistenceMockChecker >> isEnabled [
	^ self answer value
]
