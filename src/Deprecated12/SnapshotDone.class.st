"
I am announced when the image has been saved. 

Note that the announcement is done *after* the execution was frozen. So you can send me #isNewImage to know if the current image instance is the same that started the save action.
"
Class {
	#name : 'SnapshotDone',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'isNewImage'
	],
	#category : 'Deprecated12',
	#package : 'Deprecated12'
}

{ #category : 'testing' }
SnapshotDone class >> isDeprecated [
	"This announcement is deprectaed because people reacting to the startup and shut down of the image should use the SessionManager instead of this announcement.
	
	See documentation:
	https://github.com/pharo-open-documentation/pharo-wiki/blob/master/General/SessionsManagement.md "

	^ true
]

{ #category : 'instance creation' }
SnapshotDone class >> isNewImage: aBoolean [
	 ^ self new isNewImage: aBoolean
]

{ #category : 'accessing' }
SnapshotDone >> isNewImage [
	^ isNewImage
]

{ #category : 'accessing' }
SnapshotDone >> isNewImage: anObject [
	isNewImage := anObject
]

{ #category : 'abstract' }
SnapshotDone >> shouldBeConsideredByCalypsoEnvironment [
	^false
]
