Class {
	#name : #GLMTreeMorphSelectionChanged,
	#superclass : #Announcement,
	#instVars : [
		'selectionValue',
		'selectionPathValue'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #accessing }
GLMTreeMorphSelectionChanged >> selectionPathValue [
	^ selectionPathValue
]

{ #category : #accessing }
GLMTreeMorphSelectionChanged >> selectionPathValue: anObject [
	selectionPathValue := anObject
]

{ #category : #accessing }
GLMTreeMorphSelectionChanged >> selectionValue [
	^ selectionValue
]

{ #category : #accessing }
GLMTreeMorphSelectionChanged >> selectionValue: anObject [
	selectionValue := anObject
]
