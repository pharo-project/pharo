"
A MTAnnouncement is used to signal a change on a MTProject model.
"
Class {
	#name : #MTProjectModelChanged,
	#superclass : #Announcement,
	#instVars : [
		'project'
	],
	#category : #'Versionner-Core-Announcements'
}

{ #category : #creation }
MTProjectModelChanged class >> project: anObject [
	^ self new 
		project: anObject;
		yourself
]

{ #category : #accessing }
MTProjectModelChanged >> project [
	^ project
]

{ #category : #accessing }
MTProjectModelChanged >> project: anObject [
	project := anObject
]
