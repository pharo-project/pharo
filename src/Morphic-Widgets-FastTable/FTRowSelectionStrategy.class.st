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
	self isMultipleSelection ifFalse: [ ^ self ].
	self table selectIndexes: (1 to: self table numberOfRows) asArray
]

{ #category : #private }
FTRowSelectionStrategy >> selectFirst [
	self table selectRowIndex: 1
]

{ #category : #private }
FTRowSelectionStrategy >> selectLast [
	self table selectRowIndex: self table numberOfRows
]
