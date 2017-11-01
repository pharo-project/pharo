"
An Exit is an exception that quit the image without saving with a given success status.

Exit signalFaillure: 'something went wrong'
"
Class {
	#name : #Exit,
	#superclass : #Exception,
	#instVars : [
		'status'
	],
	#category : 'System-CommandLine'
}

{ #category : #'instance creation' }
Exit class >> failure [
	^ self status: 1
]

{ #category : #signalling }
Exit class >> signalFailure [
 	^ self failure signal
]

{ #category : #signalling }
Exit class >> signalFailure: aMessage [
 	^ self failure signal: aMessage
]

{ #category : #signalling }
Exit class >> signalSuccess [
 	^self success signal
]

{ #category : #signalling }
Exit class >> signalSuccess: aMessage [ 
 	^self success signal: aMessage
]

{ #category : #'instance creation' }
Exit class >> status: aPositiveInteger [
	^ self new
		status: aPositiveInteger;
		yourself
]

{ #category : #'instance creation' }
Exit class >> success [
	^ self status: 0
]

{ #category : #accessing }
Exit >> defaultAction [
 	self messageText isEmpty
		ifFalse: [ self printMessage ].
	Smalltalk exit: self status
]

{ #category : #testing }
Exit >> isSuccess [
	^self status = 0
]

{ #category : #accessing }
Exit >> printMessage [
	|stderr|
	stderr := VTermOutputDriver stderr.
	self isSuccess ifFalse: [ stderr red ].
	stderr
		nextPutAll: self messageText; lf;
		clear.
]

{ #category : #printing }
Exit >> printOn: aStream [
	super printOn: aStream.
	aStream space print: self status.
]

{ #category : #accessing }
Exit >> status [
	^ status
]

{ #category : #accessing }
Exit >> status: anInteger [
	status := anInteger .
]
