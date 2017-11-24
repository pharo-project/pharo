Class {
	#name : #GLMDropDownListMorphSelectionChanged,
	#superclass : #Announcement,
	#instVars : [
		'selectionIndex',
		'selectionValue'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #accessing }
GLMDropDownListMorphSelectionChanged >> selectionIndex [
	^ selectionIndex
]

{ #category : #accessing }
GLMDropDownListMorphSelectionChanged >> selectionIndex: anInteger [
	selectionIndex := anInteger
]

{ #category : #accessing }
GLMDropDownListMorphSelectionChanged >> selectionValue [
	^ selectionValue
]

{ #category : #accessing }
GLMDropDownListMorphSelectionChanged >> selectionValue: anObject [
	selectionValue := anObject
]
