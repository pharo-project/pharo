Class {
	#name : #GLMPagerPreviewUpdate,
	#superclass : #Announcement,
	#instVars : [
		'pageIndex'
	],
	#category : #Glamour-Morphic-Pager-Announcement
}

{ #category : #accessing }
GLMPagerPreviewUpdate >> pageIndex [
	^ pageIndex
]

{ #category : #accessing }
GLMPagerPreviewUpdate >> pageIndex: anObject [
	pageIndex := anObject
]
