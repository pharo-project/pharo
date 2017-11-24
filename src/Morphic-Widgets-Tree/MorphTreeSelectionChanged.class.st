"
Used to announce the change of a selection in the tree
"
Class {
	#name : #MorphTreeSelectionChanged,
	#superclass : #MorphTreeAnnounce,
	#instVars : [
		'selection'
	],
	#category : #'Morphic-Widgets-Tree-Announcements'
}

{ #category : #accessing }
MorphTreeSelectionChanged >> selection [
	^ selection
]

{ #category : #accessing }
MorphTreeSelectionChanged >> selection: aSelection [
	selection := aSelection
]
