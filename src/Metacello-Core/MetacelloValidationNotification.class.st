Class {
	#name : #MetacelloValidationNotification,
	#superclass : #Notification,
	#instVars : [
		'issue'
	],
	#category : #Metacello-Core-Exceptions-Notifications
}

{ #category : #exceptioninstantiator }
MetacelloValidationNotification class >> signal: aMetacelloValidationIssue [

	^ self new signal: aMetacelloValidationIssue
]

{ #category : #accessing }
MetacelloValidationNotification >> issue [
	^ issue
]

{ #category : #accessing }
MetacelloValidationNotification >> issue: anObject [
	issue := anObject
]

{ #category : #signaling }
MetacelloValidationNotification >> signal: aMetacelloValidationIssue [

	self issue: aMetacelloValidationIssue.
	^ self signal
]
