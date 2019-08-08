Class {
	#name : #FTScrollingChanged,
	#superclass : #FTAnnouncement,
	#instVars : [
		'newScrollingIndex',
		'oldScrollingIndex'
	],
	#category : #'Morphic-Widgets-FastTable-Announcement'
}

{ #category : #'instance creation' }
FTScrollingChanged class >> from: oldIndex to: newIndex [
	^ self new
		oldScrollingIndex: oldIndex;
		newScrollingIndex: newIndex;
		yourself
]

{ #category : #accessing }
FTScrollingChanged >> newScrollingIndex [
	^ newScrollingIndex
]

{ #category : #accessing }
FTScrollingChanged >> newScrollingIndex: anObject [
	newScrollingIndex := anObject
]

{ #category : #accessing }
FTScrollingChanged >> oldScrollingIndex [
	^ oldScrollingIndex
]

{ #category : #accessing }
FTScrollingChanged >> oldScrollingIndex: anObject [
	oldScrollingIndex := anObject
]
