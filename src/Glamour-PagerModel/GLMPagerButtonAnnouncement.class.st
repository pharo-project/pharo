Class {
	#name : #GLMPagerButtonAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'buttonModel'
	],
	#category : #'Glamour-PagerModel-Announcements'
}

{ #category : #accessing }
GLMPagerButtonAnnouncement >> buttonModel [
	^ buttonModel
]

{ #category : #accessing }
GLMPagerButtonAnnouncement >> buttonModel: anObject [
	buttonModel := anObject
]
