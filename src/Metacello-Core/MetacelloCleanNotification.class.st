Class {
	#name : #MetacelloCleanNotification,
	#superclass : #Notification,
	#instVars : [
		'version'
	],
	#category : #'Metacello-Core-Exceptions-Notifications'
}

{ #category : #exceptioninstantiator }
MetacelloCleanNotification class >> signal: aMetacelloVersion [

	^ self new signal: aMetacelloVersion
]

{ #category : #signaling }
MetacelloCleanNotification >> signal: aMetacelloVersion [

	self version: aMetacelloVersion.
	^ self signal
]

{ #category : #accessing }
MetacelloCleanNotification >> version [
	^ version
]

{ #category : #accessing }
MetacelloCleanNotification >> version: anObject [
	version := anObject
]
