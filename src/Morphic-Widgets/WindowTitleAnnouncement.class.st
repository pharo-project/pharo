Class {
	#name : 'WindowTitleAnnouncement',
	#superclass : 'Announcement',
	#instVars : [
		'title'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic-Widgets'
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
