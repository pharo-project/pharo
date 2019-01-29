Class {
	#name : #FTSelectionModeStrategy,
	#superclass : #Object,
	#instVars : [
		'table'
	],
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #'instance creation' }
FTSelectionModeStrategy class >> new [
	self error: 'Use #table:'
]

{ #category : #accessing }
FTSelectionModeStrategy class >> table: aFastTableMorph [
	^ self basicNew
		initializeTable: aFastTableMorph;
		yourself
]

{ #category : #initialization }
FTSelectionModeStrategy >> initializeTable: aTable [
	table := aTable.
	self initialize
]

{ #category : #testing }
FTSelectionModeStrategy >> isCellSelection [
	^ false
]

{ #category : #testing }
FTSelectionModeStrategy >> isRowSelection [
	^ false
]

{ #category : #private }
FTSelectionModeStrategy >> selectAll [
	"This method should select all elements of the table."

	self subclassResponsibility
]

{ #category : #private }
FTSelectionModeStrategy >> selectFirst [
	"This method should select the first element of the table."

	self subclassResponsibility
]

{ #category : #private }
FTSelectionModeStrategy >> selectLast [
	"This method should select the first element of the table."

	self subclassResponsibility
]

{ #category : #accessing }
FTSelectionModeStrategy >> table [
	^ table
]

{ #category : #accessing }
FTSelectionModeStrategy >> table: anObject [
	table := anObject
]
