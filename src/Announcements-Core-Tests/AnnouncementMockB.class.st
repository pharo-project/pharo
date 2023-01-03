"
This is a simple test mock
"
Class {
	#name : #AnnouncementMockB,
	#superclass : #Announcement,
	#instVars : [
		'parameter'
	],
	#category : #'Announcements-Core-Tests-Mocks'
}

{ #category : #'instance creation' }
AnnouncementMockB class >> with: anObject [
	^self new
		parameter: anObject
]

{ #category : #accessing }
AnnouncementMockB >> parameter [
	^ parameter
]

{ #category : #accessing }
AnnouncementMockB >> parameter: anObject [
	parameter := anObject
]
