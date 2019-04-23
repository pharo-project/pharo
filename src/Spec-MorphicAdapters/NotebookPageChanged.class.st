Class {
	#name : #NotebookPageChanged,
	#superclass : #NotebookPageAnnouncement,
	#instVars : [
		'oldPage'
	],
	#category : #'Spec-MorphicAdapters-Notebook'
}

{ #category : #accessing }
NotebookPageChanged >> oldPage [
	^ oldPage
]

{ #category : #accessing }
NotebookPageChanged >> oldPage: anObject [
	oldPage := anObject
]
