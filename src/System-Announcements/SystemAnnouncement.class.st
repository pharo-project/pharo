"
The root announcement for the events coming from the Pharo system.

Currently, it wraps the event that comes from the SystemChangeNotifier.
"
Class {
	#name : 'SystemAnnouncement',
	#superclass : 'Announcement',
	#instVars : [
		'properties'
	],
	#category : 'System-Announcements-System-Base',
	#package : 'System-Announcements',
	#tag : 'System-Base'
}

{ #category : 'properties' }
SystemAnnouncement >> properties [

	^ properties ifNil: [ properties := Dictionary new ]
]

{ #category : 'properties' }
SystemAnnouncement >> propertyAt: aString ifPresent: aBlockClosure [

	properties ifNil: [ ^ self ].
	^ properties at: aString ifPresent: aBlockClosure
]

{ #category : 'properties' }
SystemAnnouncement >> propertyAt: aKey put: aValue [

	self properties at: aKey put: aValue
]
