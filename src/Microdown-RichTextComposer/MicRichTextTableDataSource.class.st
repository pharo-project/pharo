"
I am a morphic table datasource for rendering tables inside rich text.
I serve only that one purpose.
"
Class {
	#name : #MicRichTextTableDataSource,
	#superclass : #FTSimpleDataSource,
	#instVars : [
		'colNames',
		'rows'
	],
	#category : #'Microdown-RichTextComposer-Table-Support'
}

{ #category : #'tests - instance creation' }
MicRichTextTableDataSource class >> headers: headers rows: theRows [
	^ self new headers: headers rows: theRows
]

{ #category : #accessing }
MicRichTextTableDataSource >> basicHeaderCellFor: column [
	| cell |
	column id ifNil: [ ^ nil ].
	cell := FTHeaderColumnCellMorph new
		column: column;
		cellInset: 5;
		addMorph: column id asTextMorph;
		yourself.

	^ cell
]

{ #category : #accessing }
MicRichTextTableDataSource >> cellColumn: column row: rowIndex [
	"Answer a morph with the cell view. I will probably return a FTCellMorph."
	| cell |
	cell  := ((rows at: rowIndex )at: (colNames at: column id )).
	^ FTCellMorph new
		addMorph: cell asTextMorph;
		yourself
]

{ #category : #private }
MicRichTextTableDataSource >> elementAt: rowIndex [
	"I am a really important method for a DataSource. I take an index and I return an object 
	 that should be displayed in the table for the index."
	rows at: rowIndex 
]

{ #category : #initialization }
MicRichTextTableDataSource >> headers: headers rows: theRows [
	rows := theRows.
	colNames := Dictionary new.
	1 to: headers size do: [ :index | colNames at: (headers at: index) put: index ]
]

{ #category : #accessing }
MicRichTextTableDataSource >> numberOfRows [
	"I return the number of elements I can display in the table when I am call."

	^ rows size
]

{ #category : #accessing }
MicRichTextTableDataSource >> rowHeight: rowIndex [
	^ (rows at: rowIndex ) max: [ :cell | cell asTextMorph height ]
]

{ #category : #sorting }
MicRichTextTableDataSource >> sortElements: aSortFunction [
	"This method should sort the elements of the datasource using the sort function as parameter."

	^ self
]

{ #category : #sorting }
MicRichTextTableDataSource >> unsortElements [
	"This method should return the elements in their initial state."

	^ self
]

{ #category : #private }
MicRichTextTableDataSource >> widthOfText: txt [
	|width|
	width := ((txt fontAt: 1) widthOfStringOrText: txt).
	1 to: txt size do: [ :index | 
		(txt at: index) = (Character value: 1)
			ifTrue: [ width := width + 
				((txt attributesAt: index) detect: [:att| att class = TextAnchor]) anchoredMorph width.
					 ]
		 ].
	^width
]
