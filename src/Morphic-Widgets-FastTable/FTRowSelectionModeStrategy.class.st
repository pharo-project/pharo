Class {
	#name : #FTRowSelectionModeStrategy,
	#superclass : #FTSelectionModeStrategy,
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #testing }
FTRowSelectionModeStrategy >> isRowSelection [
	^ true
]

{ #category : #private }
FTRowSelectionModeStrategy >> selectAll [
	self isMultipleSelection ifFalse: [ ^ self ].
	self table selectIndexes: (1 to: self table numberOfRows) asArray
]

{ #category : #private }
FTRowSelectionModeStrategy >> selectFirst [
	self table selectRowIndex: 1
]

{ #category : #private }
FTRowSelectionModeStrategy >> selectLast [
	self table selectRowIndex: self table numberOfRows
]
