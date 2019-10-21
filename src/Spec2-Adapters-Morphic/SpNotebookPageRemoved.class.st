Class {
	#name : #SpNotebookPageRemoved,
	#superclass : #SpNotebookPageAnnouncement,
	#instVars : [
		'newIndex',
		'tabs'
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

{ #category : #accessing }
SpNotebookPageRemoved >> tabs [
	^ tabs
]

{ #category : #accessing }
SpNotebookPageRemoved >> tabs: anObject [
	tabs := anObject
]
