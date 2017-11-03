"
Announcer for group announcements
"
Class {
	#name : #GroupAnnouncer,
	#superclass : #Announcer,
	#classInstVars : [
		'announcer'
	],
	#category : #Nautilus-GroupManager-Announcements
}

{ #category : #initialization }
GroupAnnouncer class >> reset [
	announcer := nil.
]

{ #category : #'instance creation' }
GroupAnnouncer class >> uniqueInstance [
	"Answer a system annoncer."
	
	^ announcer ifNil: [ announcer := self new ]
]
