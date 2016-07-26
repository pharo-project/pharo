*Graphics-Primitives
nextWordsInto: aBitmap 
	"Fill the word based buffer from my collection. 
	Stored on stream as Big Endian. Optimized for speed. 
	Read in BigEndian, then restoreEndianness."
	| blt pos source byteSize |
	collection class isBytes
		ifFalse: [^ self next: aBitmap size into: aBitmap startingAt: 1].

	byteSize := aBitmap byteSize.
	"is the test on collection basicSize \\ 4 necessary?"
	((self position bitAnd: 3) = 0 and: [ (collection basicSize bitAnd: 3) = 0])
		ifTrue: [source := collection.
			pos := self position.
			self skip: byteSize]
		ifFalse: ["forced to copy it into a buffer"
			source := self next: byteSize.
			pos := 0].

	"Now use BitBlt to copy the bytes to the bitmap."
	blt := (BitBlt
				toForm: (Form new hackBits: aBitmap))
				sourceForm: (Form new hackBits: source).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: pos // 4;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.

	"And do whatever the bitmap needs to do to convert from big-endian order."
	aBitmap restoreEndianness.

	^ aBitmap 	"May be WordArray, ColorArray, etc"
