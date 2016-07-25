displaying
fillTextBackground
	fillBlt == nil ifFalse:
		["Not right"
		fillBlt destX: line left destY: lineY
			width: line width left height: line lineHeight; copyBits].