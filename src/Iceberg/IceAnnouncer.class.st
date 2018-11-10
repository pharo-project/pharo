"
I'm the general announcer of Iceberg.
I'm an announcer that knows how to suspend just some kind of announcements (the ones that matches matching block on #suspendAllMatching:while:).

In current implementation of Iceberg, I will be used mainly for the convenience method: #suspendAllForRepository:while:

"
Class {
	#name : #IceAnnouncer,
	#superclass : #GLMAnnouncer,
	#instVars : [
		'suspendedConditions'
	],
	#category : #'Iceberg-Core'
}

{ #category : #initialization }
IceAnnouncer >> initialize [
	super initialize.
	suspendedConditions := OrderedCollection new: 5
]

{ #category : #testing }
IceAnnouncer >> isAnnouncementSuspended: anAnnouncement [
	^ (super isAnnouncementSuspended: anAnnouncement) 
		or: [ self isMatchingAnnouncementSuspended: anAnnouncement ]
]

{ #category : #testing }
IceAnnouncer >> isMatchingAnnouncementSuspended: anAnnouncement [
	suspendedConditions ifEmpty: [ ^ false ].
	^ suspendedConditions anySatisfy: [ :each | each value: anAnnouncement ]
]

{ #category : #suspending }
IceAnnouncer >> suspendAllForRepository: aRepository while: aBlock [
	^ self 
		suspendAllMatching: [ :ann | ann appliesToRepository: aRepository ] 
		while: aBlock 
]

{ #category : #suspending }
IceAnnouncer >> suspendAllMatching: matchBlock while: aBlock [
	suspendedConditions add: matchBlock.
	aBlock ensure: [ suspendedConditions remove: matchBlock ]
]
