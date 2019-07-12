"
This is a simple test mock
"
Class {
	#name : #AnnouncementMockC,
	#superclass : #AnnouncementMockB,
	#instVars : [
		'announcingCounter'
	],
	#category : #'Announcements-Core-Tests-Mocks'
}

{ #category : #accessing }
AnnouncementMockC >> announcingCounter [
	^ announcingCounter
]

{ #category : #initialization }
AnnouncementMockC >> initialize [
	super initialize.
	announcingCounter := 0
]

{ #category : #initialization }
AnnouncementMockC >> prepareForDelivery [

	announcingCounter := announcingCounter + 1
]
