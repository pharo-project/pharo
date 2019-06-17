"
I am a root strategy which subclasses defines how environment should be updated due to accepted change events.

Subclasses should implement two methods:

- announceChangesOf: aQueryResult
here subclasses should define what to do with given change

- publishCollectedChanges
here subclasses can publish all collected changes at once when their logic is to collect events instead of instant announcing them
"
Class {
	#name : #ClyEnvironmentUpdateStrategy,
	#superclass : #Object,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #updating }
ClyEnvironmentUpdateStrategy >> announceChangesOf: aQueryResult [
	self subclassResponsibility 
]

{ #category : #updating }
ClyEnvironmentUpdateStrategy >> publishCollectedChanges [
	self subclassResponsibility 
]
