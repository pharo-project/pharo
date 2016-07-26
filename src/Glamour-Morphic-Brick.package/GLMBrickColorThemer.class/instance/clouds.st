brick-colors
clouds

	^ Color brickColorRegistry at: #brickClouds
		ifAbsentPut: [ Color r: 236 g: 240 b: 241 range: 255 ]