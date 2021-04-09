Class {
	#name : #FTScrollingChangedAnnouncement,
	#superclass : #FTAnnouncement,
	#instVars : [
		'newScrollingIndex',
		'oldScrollingIndex'
	],
	#category : #'Morphic-Widgets-FastTable-Announcement'
}

{ #category : #'instance creation' }
FTScrollingChangedAnnouncement class >> from: oldIndex to: newIndex [
	^ self new
		oldScrollingIndex: oldIndex;
		newScrollingIndex: newIndex;
		yourself
]

{ #category : #accessing }
FTScrollingChangedAnnouncement >> newScrollingIndex [
	^ newScrollingIndex
]

{ #category : #accessing }
FTScrollingChangedAnnouncement >> newScrollingIndex: anObject [
	newScrollingIndex := anObject
]

{ #category : #accessing }
FTScrollingChangedAnnouncement >> oldScrollingIndex [
	^ oldScrollingIndex
]

{ #category : #accessing }
FTScrollingChangedAnnouncement >> oldScrollingIndex: anObject [
	oldScrollingIndex := anObject
]
