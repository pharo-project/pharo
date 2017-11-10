"
I am a Morph that contain visible rows in a FTTableMorph. 

Description 
------------------

I am the main Morph of the FastTable that is responsible of displaying all the rows of a Table. 
My owner need to be a FTTableMorph and I will use his dataSource to display the needed informations.

Public API and Key Messages
-----------------
		
- #updateAllRows 

- #updateExposedRows

- #ipdateHeaderRow
 
Internal Representation and Key Implementation Points.
----------------

    Instance Variables
	exposedRows:		A dictionary of index/row with all the exposed rows.
	headerRow:			When not nil contains the header row of the container.
	needsRefreshExposedRows:		A boolean that is true if the container need a refresh. 

The method #drawOn: is responsible of my rendering.
"
Class {
	#name : #FTTableContainerMorph,
	#superclass : #Morph,
	#instVars : [
		'needsRefreshExposedRows',
		'headerRow',
		'exposedRows'
	],
	#category : #Morphic-Widgets-FastTable
}

{ #category : #accessing }
FTTableContainerMorph class >> rowLeftMargin [
	"I'm keeping a small margin beween the list and the begining of a row, to enhance visibility."
	^ 1
]

{ #category : #drawing }
FTTableContainerMorph >> addResizeSplitters [
	| columnWidths nextColumn delta |
	columnWidths := self calculateColumnWidths.
	nextColumn := self left.
	delta := FTColumnResizerMorph resizerWidth / 2.
	self owner columns overlappingPairsWithIndexDo:  [ :leftColumn :rightColumn :index |
		nextColumn := nextColumn + (columnWidths at: index) + self owner intercellSpacing x.
		self addMorph: ((FTColumnResizerMorph 
			container: self 
				left: (FTDisplayColumn column: leftColumn width: (columnWidths at: index))
				right: (FTDisplayColumn column: rightColumn width: (columnWidths at: (index + 1))))
			bounds: ((nextColumn - delta)@(self top) extent: delta@(self height));
			color: Color transparent;
			yourself)	 ]
]

{ #category : #private }
FTTableContainerMorph >> calculateColumnWidths [
	"do three runs 
	- first collect defined columnwidth that fit
	- collect remaining undefined columnwidth 
	- return if all fit 
	  or collect and distribute remaining width"
	| undefinedColumnWidths widths remainingWidth |

	remainingWidth := self owner bounds width.

	widths := self owner columns
		collect: [ :c | | columnWidth | 
			columnWidth := c acquireWidth: remainingWidth. 
			remainingWidth := remainingWidth - columnWidth. 
			columnWidth ].
   "all fit - finish"		
	undefinedColumnWidths := widths count: #isZero.
   undefinedColumnWidths isZero ifTrue:[^ widths].

   "collect and distribute remaining space"
	^ widths collect: [ :c | 
		c = 0
			ifTrue: [ remainingWidth / undefinedColumnWidths]
			ifFalse: [ c ] ]
]

{ #category : #private }
FTTableContainerMorph >> calculateExactVisibleRows [
	"Answer the rows to show in list - with possible fraction"

	| visibleRows |
	visibleRows := self height / (self owner rowHeight + self owner intercellSpacing y).
	^ headerRow ifNotNil: [ visibleRows - 1 ] ifNil: [ visibleRows ]
]

{ #category : #private }
FTTableContainerMorph >> calculateMaxVisibleRows [
	"Answer the maximal number of rows to shown in list"

	^ self calculateExactVisibleRows ceiling
]

{ #category : #private }
FTTableContainerMorph >> calculateMinVisibleRows [
	"Answer the minimal fully visible number of rows to shown in list"

	^ self calculateExactVisibleRows floor
]

{ #category : #private }
FTTableContainerMorph >> calculateStartIndexWhenShowing: visibleRows [
	"Answer the first row to show when showing visibleRows rows. 
	 This works in case we are exceeding the available rows to show"

	| currentIndex startIndex |
	currentIndex := self owner showIndex.
	currentIndex + visibleRows - 1 > self owner numberOfRows ifTrue: [ 
		currentIndex := self owner numberOfRows - visibleRows + 2].
	startIndex := currentIndex max: 1.
	self owner basicMoveShowIndexTo: startIndex.
	^startIndex
	
]

{ #category : #private }
FTTableContainerMorph >> calculateVisibleRows [
	"Answer the rows to show in list. 
	 Ensures we show the maximum amount possible"

	^ self calculateMaxVisibleRows min: self owner numberOfRows
]

{ #category : #testing }
FTTableContainerMorph >> canRefreshValues [
	^ self needsRefreshExposedRows and: [ self owner isNotNil and: [ self owner hasDataSource ] ]
]

{ #category : #updating }
FTTableContainerMorph >> changed [
	self owner ifNil: [ ^ self ].
	self setNeedsRefreshExposedRows.
	super changed
]

{ #category : #drawing }
FTTableContainerMorph >> clipSubmorphs [

	^ true
]

{ #category : #private }
FTTableContainerMorph >> createResizableHeaderWith: aMorph between: leftColumn and: rightColumn [
	"Create a wrapper morph with a resizable morph et the left (so we bind two columns). 
	 This morph will be completely transparent in all terms... it acts just as a container."
	^ Morph new 
		color: Color transparent;
		clipSubmorphs: true;
		layoutPolicy: FTRowLayout new;
		bounds: aMorph bounds;
		addMorphBack: (FTColumnResizerMorph 
			container: self 
			left: leftColumn 
			right: rightColumn);
		addMorphBack: aMorph;
		yourself
]

{ #category : #initialization }
FTTableContainerMorph >> defaultColor [

	^Color transparent 
]

{ #category : #drawing }
FTTableContainerMorph >> drawOn: canvas [
	| x y cellWidth cellHeight rowsToDisplay rowSubviews highligtedRowIndexes primarySelectionIndex |
	
	super drawOn: canvas.
	self canRefreshValues ifFalse: [ ^ self ]. "Nothing to update yet"

	x := self left + self class rowLeftMargin.
	y := self top.	
	cellWidth := self width - self class rowLeftMargin.
	cellHeight := self owner rowHeight rounded. 
	highligtedRowIndexes := 
		self owner selectedRowIndexes, 
		self owner highlightedRowIndexes.
	primarySelectionIndex := self owner selectedRowIndex.
	
	"For some superweird reason, calling #calculateExposedRows here instead in #changed (where
	 it should be called) is 10x faster. Since the whole purpose of this component is speed, for 
	 now I'm calling it here and adding the #setNeedRecalculateRows mechanism. 
	 History, please forgive me."
	self updateAllRows.

	rowsToDisplay := self exposedRows.
	rowSubviews := OrderedCollection new: rowsToDisplay size + 1.
	headerRow ifNotNil: [ 
		headerRow bounds: (self left@y extent: self width@cellHeight).
		y := y + cellHeight + self owner intercellSpacing y.		
		rowSubviews add: headerRow ].
	
	rowsToDisplay keysAndValuesDo: [ :rowIndex :row | | visibleHeight |
		visibleHeight := cellHeight min: (self bottom - y). 
		row bounds: (x@y extent: cellWidth@visibleHeight).
		y := y + visibleHeight + self owner intercellSpacing y.
		
		(highligtedRowIndexes includes: rowIndex) ifTrue: [ 
			row selectionColor: (self owner colorForSelection: primarySelectionIndex = rowIndex) ].
		rowSubviews add: row ].

	"We should notify existing rows about deletion and new rows about insertion. 
	It is required to correctly manage stepping animation of cells"						
	submorphs do: [:each | 
		each privateOwner: nil; outOfWorld: self world].
	submorphs := rowSubviews asArray.
	submorphs do: [:each | each intoWorld: self world].
	
	owner isResizable 
		ifTrue: [ self addResizeSplitters ].
	

	needsRefreshExposedRows := false
]

{ #category : #drawing }
FTTableContainerMorph >> drawOnAthensCanvas: anAthensCanvas [
	self drawOnCanvasWrapperFor: anAthensCanvas
]

{ #category : #private }
FTTableContainerMorph >> exposedRows [
	"Answer a dictionary of rowIndex->row pairs"

	^ exposedRows
]

{ #category : #initialization }
FTTableContainerMorph >> initialize [ 
	super initialize.
	needsRefreshExposedRows := false
]

{ #category : #testing }
FTTableContainerMorph >> isRowIndexExceding: rowIndex [
	| headerPresentModificator nextRowIndexByPosition heightWithSpacing |
	
	headerPresentModificator := headerRow ifNotNil: [ 1 ] ifNil: [ 0 ].
	nextRowIndexByPosition := rowIndex - self owner showIndex + 1 + headerPresentModificator.
	heightWithSpacing := self owner rowHeight + self owner intercellSpacing y.
	
	^ (nextRowIndexByPosition * heightWithSpacing) > self height
]

{ #category : #testing }
FTTableContainerMorph >> isRowIndexFullyVisible: rowIndex [
	"Answer if a row is *fully* visible. That means row is completely visible (there is 
	 not hidden part)"
	^ (self isRowIndexVisible: rowIndex) 
		and: [ (self isRowIndexExceding: rowIndex) not ]
]

{ #category : #testing }
FTTableContainerMorph >> isRowIndexVisible: rowIndex [
	self exposedRows ifNil: [ ^ false ].
	^ self exposedRows includesKey: rowIndex
]

{ #category : #private }
FTTableContainerMorph >> needsRefreshExposedRows [
	^ needsRefreshExposedRows
]

{ #category : #geometry }
FTTableContainerMorph >> outerBounds [ 
	^ self bounds
]

{ #category : #accessing }
FTTableContainerMorph >> rowAndColumnIndexContainingPoint: aPoint [
	"answer a tuple containing { rowIndex. columnNumber } to be used for menus, etc. 
	 (check senders for references)"

	self exposedRows keysAndValuesDo: [ :rowIndex :row | 
		(row bounds containsPoint: aPoint) ifTrue: [ 
			row submorphs withIndexDo: [ :each :columnIndex |
				 (each bounds containsPoint: aPoint) 
					ifTrue: [ ^ { rowIndex. columnIndex } ] ] ] ].
	^ {nil. nil}
]

{ #category : #accessing }
FTTableContainerMorph >> rowIndexContainingPoint: aPoint [
	self exposedRows keysAndValuesDo: [ :rowIndex :row | 
		(row bounds containsPoint: aPoint)
			ifTrue: [ ^ rowIndex ] ].
	^ nil
]

{ #category : #private }
FTTableContainerMorph >> setNeedsRefreshExposedRows [
	needsRefreshExposedRows := true
]

{ #category : #updating }
FTTableContainerMorph >> updateAllRows [
	self owner isShowColumnHeaders 
		ifTrue: [ self updateHeaderRow ].
	self updateExposedRows.
	
]

{ #category : #updating }
FTTableContainerMorph >> updateExposedRows [
	| visibleRows numberOfColumns columns columnWidths startIndex |
	
	self canRefreshValues ifFalse: [ ^ self ].

	visibleRows := self calculateMaxVisibleRows.
	startIndex := self calculateStartIndexWhenShowing: visibleRows.
	numberOfColumns := self owner numberOfColumns.
	columns := self owner columns. 
	columnWidths := self calculateColumnWidths.

	exposedRows := SmallDictionary new.
	startIndex to: ((startIndex + visibleRows - 1) min: self owner numberOfRows) do: [ :rowIndex | 
		| row |
		row := FTTableRowMorph table: self owner.
		1 to: numberOfColumns do: [ :columnIndex | | cell |
			cell := (self owner dataSource 
				cellColumn: (columns at: columnIndex)
				row: rowIndex).  
			cell width: (columnWidths at: columnIndex).
			row addMorphBack: cell ].
		row privateOwner: self.
		exposedRows at: rowIndex put: row ]
]

{ #category : #updating }
FTTableContainerMorph >> updateHeaderRow [
	"Recalculates the header row if they are defined. 
	 Please, note that If one of the headers is nil, I assume all are nil and I return. 
	 This is probably not the best approach, but like that I enforce people defines at least 
	 a default if they want headers."
	| columnHeaders columnWidths previousDisplayColumn |

	self canRefreshValues ifFalse: [ ^ self ].

	headerRow := nil.	
	columnHeaders := Array new: self owner numberOfColumns.
	columnWidths := self calculateColumnWidths.
	
	self owner columns withIndexDo: [ :each :index | | headerCell columnWidth|
		columnWidth := columnWidths at: index.
		headerCell :=  self owner dataSource headerColumn: each. 
		headerCell ifNil: [ ^ self ]. 
		headerCell 
			color: self owner headerColor;
			width: columnWidth.
		columnHeaders at: index put: headerCell.
		previousDisplayColumn := FTDisplayColumn column: each width: columnWidth ].
	 
	headerRow := (FTTableRowMorph table: self owner)
		privateOwner: self;
		addAllMorphs: columnHeaders;
		yourself
]

{ #category : #accessing }
FTTableContainerMorph >> visibleRowMorphAtIndex: index [
	^ self exposedRows at: index
]
