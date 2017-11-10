Class {
	#name : #LazyTabPageAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'page',
		'tabs',
		'pageIndex'
	],
	#category : #Glamour-Morphic-Widgets
}

{ #category : #accessing }
LazyTabPageAnnouncement >> page [
	^ page
]

{ #category : #accessing }
LazyTabPageAnnouncement >> page: anObject [
	page := anObject
]

{ #category : #accessing }
LazyTabPageAnnouncement >> pageIndex [
	^ pageIndex
]

{ #category : #accessing }
LazyTabPageAnnouncement >> pageIndex: anObject [
	pageIndex := anObject
]

{ #category : #accessing }
LazyTabPageAnnouncement >> tabs [
	^ tabs
]

{ #category : #accessing }
LazyTabPageAnnouncement >> tabs: anObject [
	tabs := anObject
]
