"
If you want to register the same action for multiple events, simply create an AnnouncementSet using a comma: 

	Parent>>initialize 
	    super initialize. 
	    self session announcer on: AddChild, RemoveChild do: [:it | self changeChild: it child]
	
Motivation example: Often the UI is built after/independently from the model. You want to have the model raise fine-grained announcements to enable the layers on top, but sometimes it is easier in the UI to refresh everything whenever something happens.
"
Class {
	#name : #AnnouncementSet,
	#superclass : #Set,
	#category : #'Announcements-Core-Collections'
}

{ #category : #copying }
AnnouncementSet >> , anAnnouncementClass [
	self add: anAnnouncementClass
]

{ #category : #exclusion }
AnnouncementSet >> - anotherAnnouncement [
	"Create an announcement set containnig the receiver
	and anotherAnnouncement as an exclusion."

	^ AnnouncementSetWithExclusions new
		add: self;
		addExclusion: anotherAnnouncement;
		yourself
]

{ #category : #testing }
AnnouncementSet >> handlesAnnouncement: anAnnouncement [
	"If any of the set handles the announcements, subscribers should receive it."
	^ self anySatisfy: [ :each | each handlesAnnouncement: anAnnouncement ]
]
