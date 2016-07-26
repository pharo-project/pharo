*Fuel
fuelNextWordsInto: aWordObject 
	"This method is the same as nextWordsInto: but the restoreEndianness is only done if needed"
	| blt pos source byteSize |

	byteSize := aWordObject byteSize.
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
				toForm: (Form new hackBits: aWordObject))
				sourceForm: (Form new hackBits: source).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: pos // 4;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.

	^ aWordObject 	
