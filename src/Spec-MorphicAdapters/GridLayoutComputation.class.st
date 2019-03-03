"
I'm a computation to apply to a morph with GridLayout policy. 
Computation happens easily: 

- Computates number of columns and row
- Computates widths and heights (taking into account different element contraints)

check computeMorphs:bounds: to have a better idea :)
"
Class {
	#name : #GridLayoutComputation,
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
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #'instance creation' }
GridLayoutComputation class >> layout: aLayout [

	^ self basicNew 
		initializeLayout: aLayout;
		yourself 
]

{ #category : #'instance creation' }
GridLayoutComputation class >> new [

	self error: 'Use #morphs:bounds:'
]

{ #category : #private }
GridLayoutComputation >> allGridConstraints [

	^ morphs collect: [ :each | 
		each 
			valueOfProperty: #gridConstraints
			ifAbsent: [ self error: 'Constraints not defined for ', each asString ] ]
]

{ #category : #accessing }
GridLayoutComputation >> cellExtentAt: aPoint [

	^ self cellExtents at: aPoint
]

{ #category : #private }
GridLayoutComputation >> cellExtents [

	^ cellExtents
]

{ #category : #private }
GridLayoutComputation >> columnPositions [

	^ columnPositions
]

{ #category : #private }
GridLayoutComputation >> columnWidths [

	^ columnWidths
]

{ #category : #'private computation' }
GridLayoutComputation >> computeCellExtents [
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
GridLayoutComputation >> computeColumnPositions [
	"Calculate columns positions"
	
	columnPositions := Array new: numColumns.
	columnPositions at: 1 put: 0.
	2 to: numColumns do: [ :index |
		columnPositions 
			at: index 
			put: (columnPositions at: index - 1) + (columnWidths at: index - 1) + self layout columnSpacing ]
]

{ #category : #computation }
GridLayoutComputation >> computeMorphs: anArray bounds: aRect [

	morphs := anArray.
	bounds := aRect.
	
	self computeNumberOfRowsAndColumns.
	self computeWidgetSizes.
	self computeRowPositions.
	self computeColumnPositions.
	self computeCellExtents
]

{ #category : #'private computation' }
GridLayoutComputation >> computeNumberOfRowsAndColumns [
	
	numRows := 0.
	numColumns := 0.
	self allGridConstraints do: [ :each |
		numRows := numRows max: (each row + each rowSpan - 1).
		numColumns := numColumns max: (each column + each columnSpan - 1) ]
]

{ #category : #'private computation' }
GridLayoutComputation >> computeRowPositions [
	"Calculate row positions"

	rowPositions := Array new: numRows.	
	rowPositions at: 1 put: 0.
	2 to: numRows do: [ :index | 
		rowPositions 
			at: index 
			put: (rowPositions at: index - 1) + (rowHeights at: index - 1) + self layout rowSpacing ]
]

{ #category : #'private computation' }
GridLayoutComputation >> computeWidgetSizes [
	"Collect actual widget sizes"
	| fixedHeight fixedWidth |
	
	self layout isRowHomogeneous 
		ifTrue: [ fixedHeight := self maxHeight ].
	self layout isColumnHomogeneous 
		ifTrue: [ fixedWidth := self maxWidth ].
		
	rowHeights := Array new: numRows withAll: 0.
	columnWidths := Array new: numColumns withAll: 0.
	morphs do: [ :each | | properties |
		properties := each valueOfProperty: #gridConstraints.
		rowHeights 
			at: properties row 
			put: (fixedHeight ifNil: [ (rowHeights at: properties row) max: each height ]).
		columnWidths 
			at: properties column
			put: (fixedWidth ifNil: [ (columnWidths at: properties column) max: each width ] ) ]
]

{ #category : #accessing }
GridLayoutComputation >> extent [ 

	^ self columnWidths sum @ self rowHeights sum
]

{ #category : #initialization }
GridLayoutComputation >> initializeLayout: aLayout [

	self initialize.
	layout := aLayout
]

{ #category : #accessing }
GridLayoutComputation >> layout [

	^ layout
]

{ #category : #private }
GridLayoutComputation >> maxHeight [

	^ (morphs collect: #height) max
]

{ #category : #private }
GridLayoutComputation >> maxWidth [

	^ (self maxWidthOf: morphs) max: (bounds width / self numColumns) asInteger
]

{ #category : #private }
GridLayoutComputation >> maxWidthOf: aCollection [

	^ (aCollection collect: [ :each | (each valueOfProperty: #style) width ]) max
]

{ #category : #private }
GridLayoutComputation >> numColumns [

	^ numColumns
]

{ #category : #private }
GridLayoutComputation >> numRows [

	^ numRows
]

{ #category : #accessing }
GridLayoutComputation >> positionAt: aPoint [

	^ (self columnPositions at: aPoint x) @ (self rowPositions at: aPoint y) 
]

{ #category : #private }
GridLayoutComputation >> rowHeights [

	^ rowHeights
]

{ #category : #private }
GridLayoutComputation >> rowPositions [

	^ rowPositions
]
