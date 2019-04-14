"
I'm a row for a table.
All my work is to keep cells.
"
Class {
	#name : #FTTableRowMorph,
	#superclass : #FTSelectableMorph,
	#category : #'Morphic-Widgets-FastTable'
}

{ #category : #'instance creation' }
FTTableRowMorph class >> new [
	self error: 'Use #table:'
]

{ #category : #'instance creation' }
FTTableRowMorph class >> table: aTableMorph [
	^ self basicNew
		initializeTable: aTableMorph;
		yourself
]

{ #category : #'event handling' }
FTTableRowMorph >> handlesMouseOver: evt [
	self table ifNotNil: [ :table | ^ table selectionModeStrategy allowsRowSelection ].
	^ false

]

{ #category : #initialization }
FTTableRowMorph >> initialize [
	super initialize.
	self
		clipSubmorphs: true;
		layoutPolicy: FTRowLayout new
]

{ #category : #initialization }
FTTableRowMorph >> initializeTable: tableMorph [ 
	self initialize.
	self 
		cellInset: tableMorph intercellSpacing";
		layoutInset: tableMorph intercellSpacing".
	self flag: #pharoTodo. "Check if layout inset works better"
]
