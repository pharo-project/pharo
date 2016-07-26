brick-colors
whiten

	^ Color brickColorRegistry at: #brickWhiten
		ifAbsentPut: [ Color r: 241 g: 241 b: 241 range: 255 ]