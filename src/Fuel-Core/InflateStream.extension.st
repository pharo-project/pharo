Extension { #name : 'InflateStream' }

{ #category : '*Fuel-Core' }
InflateStream >> fuelNextWordsInto: aWordObject [ 
	"We can't copy bits directly from encoded streams. There is no way around copying
	to a buffer first."
	| byteSize buffer blt |
	byteSize := aWordObject byteSize.
	buffer := self next: byteSize.

	"Now use BitBlt to copy the bytes to the bitmap."
	blt := (BitBlt
		toForm: (Form new hackBits: aWordObject))
		sourceForm: (Form new hackBits: buffer).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: 0;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.

	^ aWordObject
]
