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
FTSimpleSelection >> selectIndex: rowIndex event: event [
	self toggleIndex: rowIndex
]
