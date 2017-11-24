"
I define the simple selection strategy. 
I allow tables using me to manage single selection rows.
"
Class {
	#name : #FTSimpleSelection,
	#superclass : #FTSelectionStrategy,
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #testing }
FTSimpleSelection >> isMultiple [
	^ false
]

{ #category : #accessing }
FTSimpleSelection >> selectRowIndex: rowIndex event: event [
	self toggleRowIndex: rowIndex
]
