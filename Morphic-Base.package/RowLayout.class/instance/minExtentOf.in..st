as yet unclassified
minExtentOf: aMorph in: newBounds
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