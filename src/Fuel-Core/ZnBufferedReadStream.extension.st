Extension { #name : #ZnBufferedReadStream }

{ #category : #'*Fuel-Core' }
ZnBufferedReadStream >> fuelNextWordsInto: aWordObject [
	"This method is the same as nextWordsInto: but the restoreEndianness is only done if needed"
	| byteSize source pos blt |
	byteSize := aWordObject byteSize.
	source := self next: byteSize.
	pos := 0.

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
]
