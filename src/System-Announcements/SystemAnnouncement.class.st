"
The root announcement for the events coming from the Pharo system.

Currently, it wraps the event that comes from the SystemChangeNotifier.
"
Class {
	#name : #SystemAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'timeStamp'
	],
	#category : #'System-Announcements-System-Base'
}

{ #category : #initialization }
SystemAnnouncement >> initialize [
	super initialize.
	self setTimeStamp.
]

{ #category : #private }
SystemAnnouncement >> setTimeStamp [

	timeStamp := DateAndTime current
]

{ #category : #accessing }
SystemAnnouncement >> timeStamp [

	^ timeStamp
]

{ #category : #accessing }
SystemAnnouncement >> timeStamp: aTimeStamp [

	timeStamp := aTimeStamp
]
