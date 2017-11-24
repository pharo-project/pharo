"
I define the multiple selection strategy. 
I allow tables using me to manage multiple selection rows.
"
Class {
	#name : #FTMultipleSelection,
	#superclass : #FTSelectionStrategy,
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #testing }
FTMultipleSelection >> isMultiple [
	^ true
]

{ #category : #private }
FTMultipleSelection >> selectAppendingRowIndex: rowIndex [
	| currentSelection newSelection |
	currentSelection := self table selectedRowIndexes.
	
	newSelection := (currentSelection includes: rowIndex)
		ifTrue: [ currentSelection copyWithout: rowIndex ]
		ifFalse: [ currentSelection copyWithFirst: rowIndex].
		
	self table selectRowIndexes: newSelection
]

{ #category : #accessing }
FTMultipleSelection >> selectRowIndex: rowIndex event: event [
	event shiftPressed ifTrue: [ ^ self selectRowIndexesUpTo: rowIndex ].
	event commandKeyPressed ifTrue: [ ^ self selectAppendingRowIndex: rowIndex ].
	self toggleRowIndex: rowIndex

]

{ #category : #private }
FTMultipleSelection >> selectRowIndexes: rowIndex previous: oldSelection [		
	"I ensure the selected row index is the first in selection range so I can handle it better. 
	 I do not like to assume and probably I will need the concept of 'selection', but for now 
	 let's not abuse :)"
	self table selectRowIndexes: (((oldSelection includes: rowIndex)
		ifTrue: [ oldSelection copyWithout: rowIndex ]
		ifFalse: [ oldSelection] )
		copyWithFirst: rowIndex)
]

{ #category : #private }
FTMultipleSelection >> selectRowIndexesUpTo: endIndex [
	"Selects a range of rows, from what is already selected to a new rowIndex. 
	 This is used in case of multiple selections (when holding shift)"
	| oldSelected firstIndex step |

	oldSelected := self table selectedRowIndexes.
	firstIndex :=  oldSelected 
		ifNotEmpty: [ oldSelected first ]
		ifEmpty: [ endIndex ].
	step := firstIndex <= endIndex 
		ifTrue: [ 1 ] 
		ifFalse: [ -1 ]. 

	self 
		selectRowIndexes: endIndex 
		previous: (oldSelected union: (firstIndex to: endIndex by: step))
]
