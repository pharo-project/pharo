"
I contain table rows, but opposite to my parent, I calculate my row heights allowing users to display tables with rows of different height.
"
Class {
	#name : #FTTableContainerRowNotHomogeneousMorph,
	#superclass : #FTTableContainerMorph,
	#category : #'Morphic-Widgets-FastTable'
}

{ #category : #private }
FTTableContainerRowNotHomogeneousMorph >> calculateExactVisibleRows [
	"Answer the rows to show in list - with possible fraction"
	| rowIndex maxHeight height |
	
	self table hasDataSource ifFalse: [
		^ super calculateExactVisibleRows ]. 
	
	rowIndex := self table showIndex max: 1.
	height := 0.
	maxHeight := self height.
	headerRow ifNotNil: [ maxHeight := maxHeight - headerRow height ].	
	
	[ (height < maxHeight) and: [ rowIndex <= self table dataSource numberOfRows ] ] 
	whileTrue: [ 
		height := height + (self table dataSource rowHeight: rowIndex).
		rowIndex := rowIndex + 1 ].
	
	^ rowIndex - self table showIndex
]

{ #category : #drawing }
FTTableContainerRowNotHomogeneousMorph >> rowHeight: rowIndex default: aNumber [
	
	^ self table dataSource rowHeight: rowIndex
]
