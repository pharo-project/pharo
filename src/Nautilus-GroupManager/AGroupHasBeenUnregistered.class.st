"
Notify that a group has been unregistered
"
Class {
	#name : #AGroupHasBeenUnregistered,
	#superclass : #AbstractGroupAnnouncement,
	#category : #'Nautilus-GroupManager-Announcements'
}

{ #category : #'instance creation' }
AGroupHasBeenUnregistered class >> with: aGroup [

	^ self new group: aGroup
]
