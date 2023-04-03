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
	"We do not use constant blocks here, instead use a clean block.
	Using Constant blocks with announcements makes no senese, as they do nothing."

	self announcer when: AnnouncementMockA do: [ nil yourself ] for: self
]
