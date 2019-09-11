Class {
	#name : #SpNotebookPageAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'page',
		'pageIndex'
	],
	#category : #'Spec2-Adapters-Morphic-Notebook'
}

{ #category : #accessing }
SpNotebookPageAnnouncement >> page [
	^ page
]

{ #category : #accessing }
SpNotebookPageAnnouncement >> page: anObject [
	page := anObject
]

{ #category : #accessing }
SpNotebookPageAnnouncement >> pageIndex [
	^ pageIndex
]

{ #category : #accessing }
SpNotebookPageAnnouncement >> pageIndex: anObject [
	pageIndex := anObject
]
