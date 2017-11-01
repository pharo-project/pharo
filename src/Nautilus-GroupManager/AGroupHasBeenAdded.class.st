"
Notify that a group has been added
"
Class {
	#name : #AGroupHasBeenAdded,
	#superclass : #AbstractGroupAnnouncement,
	#category : 'Nautilus-GroupManager-Announcements'
}

{ #category : #'instance creation' }
AGroupHasBeenAdded class >> group: aGroup into: anHolder [

	^ self group: aGroup from: anHolder
]
