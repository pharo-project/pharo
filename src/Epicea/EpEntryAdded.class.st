"
I announce that an entry has been added to a log.
"
Class {
	#name : #EpEntryAdded,
	#superclass : #Announcement,
	#instVars : [
		'entry'
	],
	#category : #'Epicea-Announcements'
}

{ #category : #'instance creation' }
EpEntryAdded class >> for: anEntry [

	^ self basicNew
		initializeWith: anEntry;
		yourself
]

{ #category : #accessing }
EpEntryAdded >> entry [
	^ entry
]

{ #category : #initialization }
EpEntryAdded >> initializeWith: anEntry [

	self initialize.
	entry := anEntry.
]
