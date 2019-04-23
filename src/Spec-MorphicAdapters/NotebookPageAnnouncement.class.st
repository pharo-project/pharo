Class {
	#name : #NotebookPageAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'page',
		'pageIndex'
	],
	#category : #'Spec-MorphicAdapters-Notebook'
}

{ #category : #accessing }
NotebookPageAnnouncement >> page [
	^ page
]

{ #category : #accessing }
NotebookPageAnnouncement >> page: anObject [
	page := anObject
]

{ #category : #accessing }
NotebookPageAnnouncement >> pageIndex [
	^ pageIndex
]

{ #category : #accessing }
NotebookPageAnnouncement >> pageIndex: anObject [
	pageIndex := anObject
]
