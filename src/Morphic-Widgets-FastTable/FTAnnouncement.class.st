"
I'm a base announcement for FastTable announces.
"
Class {
	#name : #FTAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'fastTable'
	],
	#category : #Morphic-Widgets-FastTable-Announcement
}

{ #category : #accessing }
FTAnnouncement >> fastTable [
	^ fastTable
]

{ #category : #accessing }
FTAnnouncement >> fastTable: aFastTable [
	fastTable := aFastTable
]
