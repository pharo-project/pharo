*Monticello-OldDataStreamCompatibility
writeRectangle: anObject
    "Write the contents of a Rectangle.  See if it can be a compact Rectangle (type=15).  Rectangles with values outside +/- 2047 were stored as normal objects (type=9).  17:22 tk"

	| ok right bottom top left acc |
	ok := true.
	(right := anObject right) > 2047 ifTrue: [ok := false].
	right < -2048 ifTrue: [ok := false].
	(bottom := anObject bottom) > 2047 ifTrue: [ok := false].
	bottom < -2048 ifTrue: [ok := false].
	(top := anObject top) > 2047 ifTrue: [ok := false].
	top < -2048 ifTrue: [ok := false].
	(left := anObject left) > 2047 ifTrue: [ok := false].
	left < -2048 ifTrue: [ok := false].
	ok := ok & left isInteger & right isInteger & top isInteger & bottom isInteger.

	ok ifFalse: [
		byteStream skip: -1; nextPut: 9; skip: 0. "rewrite type to be normal instance"
	    ^ anObject storeDataOn: self].

	acc := ((left bitAnd: 16rFFF) bitShift: 12) + (top bitAnd: 16rFFF).
	byteStream nextNumber: 3 put: acc.
	acc := ((right bitAnd: 16rFFF) bitShift: 12) + (bottom bitAnd: 16rFFF).
	byteStream nextNumber: 3 put: acc.