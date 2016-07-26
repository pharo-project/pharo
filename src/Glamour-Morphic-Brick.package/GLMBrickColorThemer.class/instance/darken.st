brick-colors
darken

	^ Color brickColorRegistry at: #brickDarken
		ifAbsentPut: [ Color r: 29 g: 29 b: 29 range: 255 ]