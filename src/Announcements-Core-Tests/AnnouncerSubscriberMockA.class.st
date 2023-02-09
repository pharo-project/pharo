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
	"we do not use constant blocks here, instead use a clean block.

	Using Constant blocks with announcements makes no senese, as they do nothing.

   For Clean Blocks, we have to the Announcments API as they  require to access the receiver of the home context, for clean blocks this has to be fixed, see https://github.com/pharo-project/pharo/issues/11220"
	self announcer when: AnnouncementMockA do: [ :evt | nil yourself " something" ]
]
