Class {
	#name : 'WindowTitleAnnouncement',
	#superclass : 'Announcement',
	#instVars : [
		'title'
	],
	#category : 'Morphic-Base',
	#package : 'Morphic-Base'
}

{ #category : 'accessing' }
WindowTitleAnnouncement class >> title: aString [

	^ self new title: aString.
]

{ #category : 'accessing' }
WindowTitleAnnouncement >> title [

	^ title
]

{ #category : 'accessing' }
WindowTitleAnnouncement >> title: aString [

	title := aString .
]
