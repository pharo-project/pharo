layout
minExtentOf: aMorph in: newBounds
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