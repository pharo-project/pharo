"
I define a strategy to handle selections. 
The sense of this class is shown in my children: I allow to handle the case of multiple and simple selection in a well designed way, instead using ifs everywhere.
"
Class {
	#name : #FTSelectionStrategy,
	#superclass : #Object,
	#instVars : [
		'table'
	],
	#category : 'Morphic-Widgets-FastTable-Selection'
}

{ #category : #'instance creation' }
FTSelectionStrategy class >> new [
	self error: 'Use #table:'
]

{ #category : #'instance creation' }
FTSelectionStrategy class >> table: aFastTableMorph [
	^ self basicNew
		initializeTable: aFastTableMorph;
		yourself
]

{ #category : #initialization }
FTSelectionStrategy >> initializeTable: aTable [
	table := aTable.
	self initialize
]

{ #category : #testing }
FTSelectionStrategy >> isMultiple [
	^ self subclassResponsibility
]

{ #category : #testing }
FTSelectionStrategy >> isSimple [
	^ self isMultiple not
]

{ #category : #accessing }
FTSelectionStrategy >> selectRowIndex: rowIndex event: event [
	self subclassResponsibility
]

{ #category : #accessing }
FTSelectionStrategy >> table [
	^ table
]

{ #category : #private }
FTSelectionStrategy >> toggleRowIndex: rowIndex [
	((self table selectedRowIndexes includes: rowIndex) 
		and: [ self table selectedRowIndexes size = 1 "just one selected"
		and: [ self table allowsDeselection ] ])
			ifTrue: [  self table selectRowIndexes: #() ]
			ifFalse: [
				self table selectRowIndex: rowIndex ]
]
