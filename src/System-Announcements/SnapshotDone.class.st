"
I am announced when the image has been saved. 

Note that the announcement is done *after* the execution was frozen. So you can send me #isNewImage to know if the current image instance is the same that started the save action.
"
Class {
	#name : #SnapshotDone,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'isNewImage'
	],
	#category : #'System-Announcements-System-Base'
}

{ #category : #'instance creation' }
SnapshotDone class >> isNewImage: aBoolean [
	 ^ self new isNewImage: aBoolean.
]

{ #category : #accessing }
SnapshotDone >> isNewImage [
	^ isNewImage
]

{ #category : #accessing }
SnapshotDone >> isNewImage: anObject [
	isNewImage := anObject
]
