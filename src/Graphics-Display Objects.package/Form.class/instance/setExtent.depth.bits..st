private
setExtent: extent depth: bitsPerPixel bits: bitmap
	"Create a virtual bit map with the given extent and bitsPerPixel."

	width := extent x asInteger.
	width < 0 ifTrue: [width := 0].
	height := extent y asInteger.
	height < 0 ifTrue: [height := 0].
	depth := bitsPerPixel.
	(bits isNil or:[self bitsSize = bitmap size]) ifFalse:[^self error:'Bad dimensions'].
	bits := bitmap