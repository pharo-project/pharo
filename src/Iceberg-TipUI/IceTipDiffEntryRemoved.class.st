"
I announce when an entry (a method, a class or a package) has been reverted. 

I'm used to refresh diff panels without recalculate everything again.  
"
Class {
	#name : #IceTipDiffEntryRemoved,
	#superclass : #IceTipAnnouncement,
	#instVars : [
		'entry'
	],
	#category : #'Iceberg-TipUI-Announcements'
}

{ #category : #'instance creation' }
IceTipDiffEntryRemoved class >> entry: anObject [
	^ self new entry: anObject
]

{ #category : #accessing }
IceTipDiffEntryRemoved >> entry [
	^ entry
]

{ #category : #accessing }
IceTipDiffEntryRemoved >> entry: anObject [
	entry := anObject
]
