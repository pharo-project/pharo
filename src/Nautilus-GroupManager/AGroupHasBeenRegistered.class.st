"
Notify that a group has been registered
"
Class {
	#name : #AGroupHasBeenRegistered,
	#superclass : #AbstractGroupAnnouncement,
	#category : 'Nautilus-GroupManager-Announcements'
}

{ #category : #'instance creation' }
AGroupHasBeenRegistered class >> with: aGroup [

	^ self new group: aGroup
]
