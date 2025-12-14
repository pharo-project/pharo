Class {
	#name : 'MetacelloValidationFailure',
	#superclass : 'MetacelloError',
	#instVars : [
		'issues'
	],
	#category : 'Metacello-Core-Exceptions',
	#package : 'Metacello-Core',
	#tag : 'Exceptions'
}

{ #category : 'instance creation' }
MetacelloValidationFailure class >> issues: anObject message: aString [

	^ self new
		  issues: anObject;
		  signal: aString , ' Issues: ' , (anObject collect: [ :error | error explanation ]) printString
]

{ #category : 'accessing' }
MetacelloValidationFailure >> issues [
	^ issues
]

{ #category : 'accessing' }
MetacelloValidationFailure >> issues: anObject [
	issues := anObject
]
