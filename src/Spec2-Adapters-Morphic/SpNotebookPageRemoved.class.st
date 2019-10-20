Class {
	#name : #SpNotebookPageRemoved,
	#superclass : #SpNotebookPageAnnouncement,
	#instVars : [
		'newIndex'
	],
	#category : #'Spec2-Adapters-Morphic-Notebook'
}

{ #category : #accessing }
SpNotebookPageRemoved >> newIndex [
	^ newIndex
]

{ #category : #accessing }
SpNotebookPageRemoved >> newIndex: anObject [
	newIndex := anObject
]
