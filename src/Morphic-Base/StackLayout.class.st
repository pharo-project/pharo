Class {
	#name : #StackLayout,
	#superclass : #LayoutPolicy,
	#instVars : [
		'cachedMinExtent',
		'cachedMinExtents'
	],
	#category : 'Morphic-Base-Layouts'
}

{ #category : #private }
StackLayout >> flushLayoutCache [
	"Flush any cached information associated with the receiver."
	
	cachedMinExtent := nil
]

{ #category : #layout }
StackLayout >> layout: aMorph in: layoutBounds [
	"Compute the layout for the given morph based on the new bounds.
	Supports submorph hResizing, vResizing, cellInset, cellPositioning
	(top, *center, bottom*)."

	|props cell box pos vr newBounds minExt|
	aMorph submorphs ifEmpty: [^self].
	props := aMorph assureTableProperties.
	minExt := self minExtentOf: aMorph in: layoutBounds.
	newBounds := layoutBounds origin extent: (layoutBounds extent max: minExt).
	cell := newBounds.
	aMorph submorphs with: cachedMinExtents do: [:m :ext |
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
								m bounds: box]]]
]

{ #category : #layout }
StackLayout >> minExtentOf: aMorph in: newBounds [
	"Return the minimal size aMorph's children would require given the new bounds"
	
	|min extent|
	cachedMinExtent ifNotNil: [^cachedMinExtent].
	aMorph submorphs ifEmpty: [^0 @ 0].
	extent := 0@0.
	cachedMinExtents := aMorph submorphs collect: [:m |
		min := m minExtent.
		extent := extent max: min.
		min].
	^cachedMinExtent := extent
]
