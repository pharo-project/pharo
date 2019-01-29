Class {
	#name : #FTRowSelectionStrategy,
	#superclass : #FTSelectionModeStrategy,
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #testing }
FTRowSelectionStrategy >> isRowSelection [
	^ true
]

{ #category : #private }
FTRowSelectionStrategy >> selectAll [
	self table selectAllRowIndexes
]

{ #category : #private }
FTRowSelectionStrategy >> selectFirst [
	self table selectRowIndex: 1
]

{ #category : #private }
FTRowSelectionStrategy >> selectLast [
	self table selectRowIndex: self table numberOfRows
]
