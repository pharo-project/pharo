"
I'm a computation to apply to a morph with GridLayout policy. 
Computation happens easily: 

- Computates number of columns and row
- Computates widths and heights (taking into account different element contraints)

check computeMorphs:bounds: to have a better idea :)
"
Class {
	#name : #SpMorphicGridLayoutComputation,
	#superclass : #LayoutPolicy,
	#instVars : [
		'morphs',
		'numRows',
		'numColumns',
		'rowHeights',
		'columnWidths',
		'rowPositions',
		'columnPositions',
		'cellExtents',
		'bounds',
		'layout'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #'instance creation' }
SpMorphicGridLayoutComputation class >> layout: aLayout [

	^ self basicNew 
		initializeLayout: aLayout;
		yourself 
]

{ #category : #'instance creation' }
SpMorphicGridLayoutComputation class >> new [

	self error: 'Use #morphs:bounds:'
]

{ #category : #private }
SpMorphicGridLayoutComputation >> allGridConstraints [

	^ morphs collect: [ :each | 
		each 
			valueOfProperty: #gridConstraints
			ifAbsent: [ self error: 'Constraints not defined for ', each asString ] ]
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> baseColumnWidth [

	^ (bounds width / numColumns) asInteger
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> baseRowHeight [

	^ (bounds height / numRows) asInteger
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> calculateColumnWidths [

	^ layout isColumnHomogeneous
		ifTrue: [ self calculateHomogeneousColumnWidths ]
		ifFalse: [ self calculateNotHomogeneousColumnWidths ]
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> calculateHomogeneousColumnWidths [
	"columns are homogeneous. 
	 - width will be bounds width/numColumns UNLESS there is a column with a min width bigger 
	   than that number.
	 - if min width > bounds width/numColumns, then all columns becomw width = min width (to 
		make them homogeneous)"
	| columnWidth |
	
	columnWidth := 0.
	morphs do: [ :each | 
		columnWidth := columnWidth max: (self minWidthOf: each) ].
	
	^ Array new: numColumns withAll: (self baseColumnWidth max: columnWidth)
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> calculateHomogeneousRowHeights [
	"roes are homogeneous. 
	 - height will be bounds height/numRows UNLESS there is a row with a min height bigger 
	   than that number.
	 - if min height > bounds height/numRows, then all rows become height = min height (to 
		make them homogeneous)"
	| rowHeight |
	
	rowHeight := 0.
	morphs do: [ :each | 
		rowHeight := rowHeight max: (self minHeightOf: each) ].
	
	^ Array new: numRows withAll: (self baseRowHeight max: rowHeight)
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> calculateNotHomogeneousColumnWidths [
	"column widths are 
	 - the max width of cells of that column. EXCEPT if
	 - the column is expandable, in which case, they will receive the remaining width 
	   after assigning the others (a minimum if there is no remaining place)"
	| newColumnWidths expandables |
	
	newColumnWidths := Array new: numColumns withAll: 0.
	
	"Assign non expandable values"
	morphs do: [ :each | | column |
		column := (each valueOfProperty: #gridConstraints) column.
		(self isColumnExpandable: column) ifFalse: [ 
			newColumnWidths 
				at: column
				put: ((newColumnWidths at: column) max: ((self styleOf: each) width)) ] ].
		
	"now I count the zeroes, if there are remaining means we need to assign the remaining 
	 width (divided between the remaining)"
	expandables := newColumnWidths count: [ :each | each isZero ].
	expandables > 0 ifTrue: [ | width accumulatedWidth |
		accumulatedWidth := (newColumnWidths reject: [ :each | each isZero ]) 
			ifNotEmpty: [ :nonZeroWidths | nonZeroWidths sum ]
			ifEmpty: [ 0 ]. 
		width := ((bounds width - accumulatedWidth) / expandables) asInteger.
		newColumnWidths := newColumnWidths collect: [ :each | 
			each = 0 
				ifTrue: [ width ]
				ifFalse: [ each ] ] ].
		
	^ newColumnWidths
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> calculateNotHomogeneousRowHeights [
	"column heights are 
	 - the max height of cells of that row"
	| newRowHeights |	
	
	newRowHeights := Array new: numRows withAll: 0.
	morphs do: [ :each | | row |
		row := (each valueOfProperty: #gridConstraints) row.
		newRowHeights 
			at: row 
			put: ((newRowHeights at: row) max: each height) ].
		
	^ newRowHeights
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> calculateRowHeights [
		
	^ layout isRowHomogeneous 
		ifTrue: [ self calculateHomogeneousRowHeights ]
		ifFalse: [ self calculateNotHomogeneousRowHeights ]
]

{ #category : #accessing }
SpMorphicGridLayoutComputation >> cellExtentAt: aPoint [

	^ self cellExtents at: aPoint
]

{ #category : #private }
SpMorphicGridLayoutComputation >> cellExtents [

	^ cellExtents
]

{ #category : #private }
SpMorphicGridLayoutComputation >> columnPositions [

	^ columnPositions
]

{ #category : #private }
SpMorphicGridLayoutComputation >> columnWidths [

	^ columnWidths
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> computeCellExtents [
	"Calculate cell extents"

	cellExtents := SmallDictionary new.
	self allGridConstraints do: [ :constraint | | width height |
		width := (columnWidths 
			copyFrom: constraint column 
			to: constraint column + constraint columnSpan - 1) 
			sum.
		height := (rowHeights 
			copyFrom: constraint row 
			to: constraint row + constraint rowSpan - 1) 
			sum.
		cellExtents 
			at: constraint position 
			put: width@height ]
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> computeColumnPositions [
	"Calculate columns positions"
	
	columnPositions := Array new: numColumns.
	columnPositions at: 1 put: 0.
	2 to: numColumns do: [ :index |
		columnPositions 
			at: index 
			put: (columnPositions at: index - 1) + (columnWidths at: index - 1) + self layout columnSpacing ]
]

{ #category : #computation }
SpMorphicGridLayoutComputation >> computeMorphs: anArray bounds: aRect [

	morphs := anArray.
	bounds := aRect.
	
	self computeNumberOfRowsAndColumns.
	self computeWidgetSizes.
	self computeRowPositions.
	self computeColumnPositions.
	self computeCellExtents
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> computeNumberOfRowsAndColumns [
	
	numRows := 0.
	numColumns := 0.
	self allGridConstraints do: [ :each |
		numRows := numRows max: (each row + each rowSpan - 1).
		numColumns := numColumns max: (each column + each columnSpan - 1) ]
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> computeRowPositions [
	"Calculate row positions"

	rowPositions := Array new: numRows.	
	rowPositions at: 1 put: 0.
	2 to: numRows do: [ :index | 
		rowPositions 
			at: index 
			put: (rowPositions at: index - 1) + (rowHeights at: index - 1) + self layout rowSpacing ]
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> computeWidgetSizes [
	"Collect actual widget sizes"

	rowHeights := self calculateRowHeights.
	columnWidths := self calculateColumnWidths
]

{ #category : #accessing }
SpMorphicGridLayoutComputation >> extent [ 

	^ self columnWidths sum @ self rowHeights sum
]

{ #category : #initialization }
SpMorphicGridLayoutComputation >> initializeLayout: aLayout [

	self initialize.
	layout := aLayout
]

{ #category : #private }
SpMorphicGridLayoutComputation >> isColumnExpandable: colNumber [

	^ (layout columnConstraintsAt: colNumber)
		ifNotNil: [ :c | c isExpand ]
		ifNil: [ false ]
]

{ #category : #private }
SpMorphicGridLayoutComputation >> isRowExpandable: rowNumber [

	^ (layout rowConstraintsAt: rowNumber)
		ifNotNil: [ :c | c isExpand ]
		ifNil: [ false ]
]

{ #category : #accessing }
SpMorphicGridLayoutComputation >> layout [

	^ layout
]

{ #category : #private }
SpMorphicGridLayoutComputation >> maxWidthOf: aCollection [

	^ (aCollection collect: [ :each | (each valueOfProperty: #style) width ]) max
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> minHeightOf: aMorph [ 

	^ (self styleOf: aMorph) minHeight ifNil: [ 0 ]
]

{ #category : #'private computation' }
SpMorphicGridLayoutComputation >> minWidthOf: aMorph [ 

	^ (self styleOf: aMorph) minWidth ifNil: [ 0 ]
]

{ #category : #private }
SpMorphicGridLayoutComputation >> numColumns [

	^ numColumns
]

{ #category : #private }
SpMorphicGridLayoutComputation >> numRows [

	^ numRows
]

{ #category : #accessing }
SpMorphicGridLayoutComputation >> positionAt: aPoint [

	^ (self columnPositions at: aPoint x) @ (self rowPositions at: aPoint y) 
]

{ #category : #private }
SpMorphicGridLayoutComputation >> rowHeights [

	^ rowHeights
]

{ #category : #private }
SpMorphicGridLayoutComputation >> rowPositions [

	^ rowPositions
]

{ #category : #private }
SpMorphicGridLayoutComputation >> styleOf: aMorph [ 

	^ aMorph valueOfProperty: #style
]
