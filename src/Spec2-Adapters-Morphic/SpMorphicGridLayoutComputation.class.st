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
	| fixedHeight fixedWidth |
	
	self layout isRowHomogeneous 
		ifTrue: [ fixedHeight := (bounds height / numRows) asInteger ].
	self layout isColumnHomogeneous 
		ifTrue: [ fixedWidth := (bounds width / numColumns) asInteger ].
		
	rowHeights := Array new: numRows withAll: 0.
	columnWidths := Array new: numColumns withAll: 0.
	morphs do: [ :each | | properties |
		properties := each valueOfProperty: #gridConstraints.
		rowHeights 
			at: properties row 
			put: (fixedHeight ifNil: [ (rowHeights at: properties row) max: each height ]).
		columnWidths 
			at: properties column
			put: (fixedWidth ifNil: [ (columnWidths at: properties column) max: each width ]) ]
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

{ #category : #accessing }
SpMorphicGridLayoutComputation >> layout [

	^ layout
]

{ #category : #private }
SpMorphicGridLayoutComputation >> maxWidthOf: aCollection [

	^ (aCollection collect: [ :each | (each valueOfProperty: #style) width ]) max
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
