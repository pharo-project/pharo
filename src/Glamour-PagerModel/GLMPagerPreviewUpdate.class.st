Class {
	#name : #GLMPagerPreviewUpdate,
	#superclass : #Announcement,
	#instVars : [
		'pageIndex'
	],
	#category : #'Glamour-PagerModel-Announcements'
}

{ #category : #testing }
GLMPagerPreviewUpdate class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMPagerPreviewUpdate >> pageIndex [
	^ pageIndex
]

{ #category : #accessing }
GLMPagerPreviewUpdate >> pageIndex: anObject [
	pageIndex := anObject
]
