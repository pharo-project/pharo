"
I am a Morphic table used by `MicRichTextComposer>>visitTable:` to render tables. I serve no other purpose. 
"
Class {
	#name : #MicRichTextTable,
	#superclass : #FTTableMorph,
	#category : #'Microdown-RichTextComposer-Table-Support'
}

{ #category : #'instance creation' }
MicRichTextTable class >> headers: headers rows: rows [
	"headers is an array of column names, and rows are an array of array of string"
	^ self new headers: headers rows: rows
]

{ #category : #adding }
MicRichTextTable >> addHeaders: headers with: renderedRows [
	|totalWidth totalHeight  |
	totalWidth := 0.
	totalHeight := 0.
	1 to: headers size do:[ :colIndex | |header colRows colWidth colHeight|
		header := headers at: colIndex.
		colRows := renderedRows collect: [ :row | row at: colIndex  ].
		colWidth := (header asTextMorph width) 
					max: (colRows collect: [:cell| cell asTextMorph width]) max.
		totalWidth := totalWidth + colWidth.
		colHeight := header asTextMorph height 
				+ ((colRows collect: [:cell| cell asTextMorph height]) sum).
		totalHeight := totalHeight max: colHeight.
		self addColumn: ((FTColumn id: header) width: colWidth +5 )
	].
	self extent: (totalWidth + (headers size * 5)) @ (totalHeight+(renderedRows size +1 *5) )
]

{ #category : #initialization }
MicRichTextTable >> headers: headers rows: rows [
	self
		addHeaders: headers with: rows ;
		dataSource: (MicRichTextTableDataSource headers: headers rows: rows);
		beRowNotHomogeneous.
		
	
]

{ #category : #rendering }
MicRichTextTable >> renderCell: aCell [
	"a cell is an array of nodes. Each element should be rendered and concatenated"
	^ aCell inject: Text new into: [ :txt :part | 
		txt, (MicRichTextComposer new visit: part)
		 ]
]

{ #category : #private }
MicRichTextTable >> resizeAllSubviews [
	"This method is just like its super, except it prevents the vertical scroll bar to appear"
	self resizeContainer.
	self container setNeedsRefreshExposedRows.
	self container updateExposedRows.
	function isExplicit
		ifTrue: [ function resizeWidget ]
]
