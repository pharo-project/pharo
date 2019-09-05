"
I am a mock class for testing in announcers
"
Class {
	#name : #AnnouncerSubscriberMockA,
	#superclass : #Object,
	#instVars : [
		'announcer'
	],
	#category : #'Announcements-Core-Tests-Mocks'
}

{ #category : #accessing }
AnnouncerSubscriberMockA >> announcer [
	^ announcer
]

{ #category : #accessing }
AnnouncerSubscriberMockA >> announcer: anAnnouncer [
	announcer := anAnnouncer
]

{ #category : #events }
AnnouncerSubscriberMockA >> registerEvents [
	self announcer when: AnnouncementMockA do: [ :evt | " something" ].
]
