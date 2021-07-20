Class {
	#name : #GLMTreeMorphStrongSelectionChanged,
	#superclass : #Announcement,
	#instVars : [
		'strongSelectionValue'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #testing }
GLMTreeMorphStrongSelectionChanged class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMTreeMorphStrongSelectionChanged >> strongSelectionValue [
	^ strongSelectionValue
]

{ #category : #accessing }
GLMTreeMorphStrongSelectionChanged >> strongSelectionValue: anObject [
	strongSelectionValue := anObject
]
