"
A NautilusGroupSelected is raised when a group is selected
"
Class {
	#name : #NautilusGroupSelected,
	#superclass : #Announcement,
	#instVars : [
		'group'
	],
	#category : #'NautilusCommon-Announcements'
}

{ #category : #'instance creation' }
NautilusGroupSelected class >> group: aGroup [

	^ self new group: aGroup
]

{ #category : #accessing }
NautilusGroupSelected >> group [

		^ group
]

{ #category : #accessing }
NautilusGroupSelected >> group: anObject [
	
		group := anObject
]
