"
I define the simple selection strategy. 
I allow tables using me to manage single selection rows.
"
Class {
	#name : 'FTSimpleSelectionStrategy',
	#superclass : 'FTSelectionStrategy',
	#category : 'Morphic-Widgets-FastTable-Selection',
	#package : 'Morphic-Widgets-FastTable',
	#tag : 'Selection'
}

{ #category : 'testing' }
FTSimpleSelectionStrategy >> isMultiple [
	^ false
]

{ #category : 'accessing' }
FTSimpleSelectionStrategy >> selectIndex: rowIndex event: event [
	self toggleIndex: rowIndex
]
