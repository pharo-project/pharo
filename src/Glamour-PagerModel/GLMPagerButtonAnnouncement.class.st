Class {
	#name : #GLMPagerButtonAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'buttonModel'
	],
	#category : #'Glamour-PagerModel-Announcements'
}

{ #category : #testing }
GLMPagerButtonAnnouncement class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMPagerButtonAnnouncement >> buttonModel [
	^ buttonModel
]

{ #category : #accessing }
GLMPagerButtonAnnouncement >> buttonModel: anObject [
	buttonModel := anObject
]
