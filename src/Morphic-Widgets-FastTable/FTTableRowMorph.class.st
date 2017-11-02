"
I'm a row for a table.
All my work is to keep cells.
"
Class {
	#name : #FTTableRowMorph,
	#superclass : #Morph,
	#instVars : [
		'selectionColor'
	],
	#category : 'Morphic-Widgets-FastTable'
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

{ #category : #initialize }
FTTableRowMorph >> defaultColor [
	^Color transparent 
]

{ #category : #'event handling' }
FTTableRowMorph >> handlesMouseOver: evt [

	^true
]

{ #category : #initialization }
FTTableRowMorph >> initialize [ 
	super initialize.
	self
		clipSubmorphs:true;
		layoutPolicy: FTRowLayout new.
		
	selectionColor := self defaultColor
	
]

{ #category : #initialization }
FTTableRowMorph >> initializeTable: tableMorph [ 
	self initialize.
	self 
		cellInset: tableMorph intercellSpacing";
		layoutInset: tableMorph intercellSpacing".
	self flag: #todo. "Check if layout inset works better"
]

{ #category : #'event handling' }
FTTableRowMorph >> mouseEnter: evt [

	self color: (
		selectionColor isTransparent 
			ifTrue: [ self mouseOverColor ]
			ifFalse: [ self mouseOverColor mixed: 0.5 with: selectionColor ]
	)
]

{ #category : #'event handling' }
FTTableRowMorph >> mouseLeave: evt [
	
	self color: self selectionColor 
]

{ #category : #initialize }
FTTableRowMorph >> mouseOverColor [
	^ Smalltalk ui theme lightBackgroundColor
]

{ #category : #initialize }
FTTableRowMorph >> selectionColor [

	^selectionColor
]

{ #category : #accessing }
FTTableRowMorph >> selectionColor: aColor [

	selectionColor := aColor.
	self color: selectionColor
]
