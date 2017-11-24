Class {
	#name : #MetacelloValidationFailure,
	#superclass : #Error,
	#instVars : [
		'issues'
	],
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #'instance creation' }
MetacelloValidationFailure class >> issues: anObject message: aString [
    ^ self new
        issues: anObject;
        signal: aString
]

{ #category : #accessing }
MetacelloValidationFailure >> issues [
	^ issues
]

{ #category : #accessing }
MetacelloValidationFailure >> issues: anObject [
	issues := anObject
]
