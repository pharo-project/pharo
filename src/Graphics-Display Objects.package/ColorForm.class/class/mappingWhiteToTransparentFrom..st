instance creation
mappingWhiteToTransparentFrom: aFormOrCursor
	"Return a ColorForm copied from the given Form or Cursor with white mapped to transparent."

	| f map |
	aFormOrCursor depth <= 8 ifFalse: [
		^ self error: 'argument depth must be 8-bits per pixel or less'].
	(aFormOrCursor isColorForm) ifTrue: [
		f := aFormOrCursor deepCopy.
		map := aFormOrCursor colors.
	] ifFalse: [
		f := ColorForm extent: aFormOrCursor extent depth: aFormOrCursor depth.
		f copyBits: aFormOrCursor boundingBox
			from: aFormOrCursor
			at: 0@0
			clippingBox: aFormOrCursor boundingBox
			rule: Form over
			fillColor: nil.
		map := Color indexedColors copyFrom: 1 to: (1 bitShift: aFormOrCursor depth)].
	map := map collect: [:c |
		c = Color white ifTrue: [Color transparent] ifFalse: [c]].
	f colors: map.
	^ f
