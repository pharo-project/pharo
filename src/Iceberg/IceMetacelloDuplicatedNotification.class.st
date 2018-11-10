Class {
	#name : #IceMetacelloDuplicatedNotification,
	#superclass : #MetacelloAllowProjectUpgrade,
	#instVars : [
		'builder'
	],
	#category : #'Iceberg-Adapters'
}

{ #category : #accessing }
IceMetacelloDuplicatedNotification >> builder [
	^ builder
]

{ #category : #accessing }
IceMetacelloDuplicatedNotification >> builder: anObject [
	builder := anObject
]

{ #category : #accessing }
IceMetacelloDuplicatedNotification >> defaultAction [
    "Default for Upgrade is to allow"

	^ self useIncoming
]

{ #category : #accessing }
IceMetacelloDuplicatedNotification >> existingProjectRegistration [

	| repository |
	repository := IceLibgitRepository new
		location: builder locationToUse.
	^ IceMetacelloProjectRegistration new
		version: repository head description;
		yourself
]

{ #category : #accessing }
IceMetacelloDuplicatedNotification >> newProjectRegistration [

	^ IceMetacelloProjectRegistration new
		version: builder commitishName;
		yourself
]

{ #category : #accessing }
IceMetacelloDuplicatedNotification >> projectName [
	
	^ builder locationToUse basename
]

{ #category : #'exception handling' }
IceMetacelloDuplicatedNotification >> useIncoming [
	^ self resume: self builder addLocalRepositoryAndCheckout
]

{ #category : #'exception handling' }
IceMetacelloDuplicatedNotification >> useLoaded [
	^ self resume: self builder addLocalRepository
]
