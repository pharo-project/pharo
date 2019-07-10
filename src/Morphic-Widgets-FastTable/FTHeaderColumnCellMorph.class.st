"
I represent a column header cell morph, i handle mouse click and double to trigger the needed functionalities.
"
Class {
	#name : #FTHeaderColumnCellMorph,
	#superclass : #FTCellMorph,
	#instVars : [
		'column',
		'allowSort',
		'isColumnOrderedFromLeast',
		'isColumnOrderedFromGreatest'
	],
	#category : #'Morphic-Widgets-FastTable'
}

{ #category : #accessing }
FTHeaderColumnCellMorph >> column [
	^ column
]

{ #category : #accessing }
FTHeaderColumnCellMorph >> column: aColumn [
	column  := aColumn 
]

{ #category : #'event handling' }
FTHeaderColumnCellMorph >> handlesMouseDown: event [
	^ true

]

{ #category : #'event handling' }
FTHeaderColumnCellMorph >> handlesMouseOver: event [
	^ false

]

{ #category : #'event handling' }
FTHeaderColumnCellMorph >> mouseDown: event [
	self column sortDataSource: self table dataSource
]
