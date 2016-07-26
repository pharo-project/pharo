private
copyWordObjectToBuffer: aWordObject
	| blt |
	blt := (BitBlt
		toForm: (Form new hackBits: self buffer))
		sourceForm: (Form new hackBits: aWordObject).
	blt combinationRule: Form over. "store"
	blt 
		sourceX: 0;
		sourceY: 0;
		height: aWordObject byteSize // 4;
		width: 4.
	blt 
		destX: 0;
		destY:  position // 4.
	blt copyBits