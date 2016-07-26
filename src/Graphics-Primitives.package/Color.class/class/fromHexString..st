instance creation
fromHexString: aColorHex
	| green red blue |
	red := (Integer readFrom: (aColorHex first: 2) base: 16) / 255.
	green := (Integer readFrom: (aColorHex copyFrom: 3 to: 4) base: 16) / 255.
	blue := (Integer readFrom: (aColorHex last: 2) base: 16) / 255.
	^self r: red g: green b: blue