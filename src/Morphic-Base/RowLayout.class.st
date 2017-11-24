"
A simple row layout.
"
Class {
	#name : #RowLayout,
	#superclass : #LayoutPolicy,
	#instVars : [
		'cachedMinExtent',
		'cachedMinExtents'
	],
	#category : #'Morphic-Base-Layouts'
}

{ #category : #'as yet unclassified' }
RowLayout >> flushLayoutCache [
	"Flush any cached information associated with the receiver."
	
	cachedMinExtent := nil
]

{ #category : #'as yet unclassified' }
RowLayout >> layout: aMorph in: layoutBounds [
	"Compute the layout for the given morph based on the new bounds.
	Supports submorph hResizing, vResizing, cellInset, cellPositioning
	(top, *center, bottom*) and listCentering."

	| props spare extra height inset newBounds minExt fillCount x width|
	aMorph submorphs ifEmpty: [^self].
	props := aMorph assureTableProperties.
	minExt := (aMorph minWidth@aMorph minHeight - aMorph extent + aMorph layoutBounds extent) max: 0@0.
	newBounds := layoutBounds origin extent: (layoutBounds extent max: minExt).
	width := 0.
	width := (self minExtentOf: aMorph in: newBounds) x.
	spare := newBounds width - width max: 0.
	fillCount := 0.
	spare > 0
		ifTrue: [fillCount := aMorph submorphs inject: 0 into: [:tot :m |
					tot + (m hResizing == #spaceFill ifTrue: [1] ifFalse: [0])].
				extra := fillCount = 0
					ifTrue: [0]
					ifFalse: [spare // fillCount].
				spare := spare - (fillCount - 1 * extra)]
		ifFalse: [extra := 0].
	x := fillCount > 0
		ifTrue: [newBounds left]
		ifFalse: [props listCentering == #center
					ifTrue: [newBounds center x - (width // 2)]
					ifFalse: [props listCentering == #bottomRight
								ifTrue: [newBounds right - width]
								ifFalse: [newBounds left]]].
	height := newBounds height.
	inset := props cellInset isPoint ifTrue: [props cellInset x] ifFalse: [props cellInset]. 
	aMorph submorphs with: cachedMinExtents do: [:m :ext | | pos vr box cell |
		width := m hResizing == #spaceFill
			ifTrue: [fillCount := fillCount - 1.
					ext x + (fillCount > 0
						ifTrue: [spare]
						ifFalse: [extra])]
			ifFalse: [ext x].
		cell := x@newBounds top extent: width@height.
		((vr := m vResizing) == #shrinkWrap or: [m bounds ~= cell])
			ifTrue: [((vr == #shrinkWrap) not and: [m extent = cell extent])
						ifTrue: [m position: cell origin]
						ifFalse: [box := m bounds.
								m hResizing == #spaceFill 
									ifTrue: [box := cell origin extent: cell width @ box height].
								vr  == #spaceFill
									ifTrue: [box := box origin extent: box width @ cell height].
								vr  == #shrinkWrap
									ifTrue:[box := box origin extent: box width @ ext y].
								pos := props cellPositioning.
								box := box align: (box perform: pos) with: (cell perform: pos).
								m bounds: box]].
		x := x + width + inset]
]

{ #category : #'as yet unclassified' }
RowLayout >> minExtentOf: aMorph in: newBounds [
	"Return the minimal size aMorph's children would require given the new bounds"
	
	| props extent|
	cachedMinExtent ifNotNil: [^cachedMinExtent].
	aMorph submorphs ifEmpty: [^aMorph minWidth @ aMorph minHeight].
	extent := 0@0.
	cachedMinExtents := aMorph submorphs collect: [:m | | min |
		min := m minExtent.
		extent := extent x + min x @ (extent y max: min y).
		min].
	props := aMorph assureTableProperties.
	^cachedMinExtent := extent + (aMorph submorphs size - 1 * 
		(props cellInset isPoint ifTrue: [props cellInset x] ifFalse: [props cellInset]) @ 0)
]
