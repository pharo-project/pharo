conversions
indexInMap: aColorMap
	"Return the index corresponding to this color in the given color map. RGB colors are truncated to 3-, 4-, or 5-bits per color component when indexing into such a colorMap.  "

	aColorMap size = 2 ifTrue: [^ (self pixelValueForDepth: 1) + 1].
	aColorMap size = 4 ifTrue: [^ (self pixelValueForDepth: 2) + 1].
	aColorMap size = 16 ifTrue: [^ (self pixelValueForDepth: 4) + 1].
	aColorMap size = 256 ifTrue: [^ (self pixelValueForDepth: 8) + 1].
	aColorMap size = 512 ifTrue: [^ (self pixelValueForDepth: 9) + 1].
	aColorMap size = 4096 ifTrue: [^ (self pixelValueForDepth: 12) + 1].
	aColorMap size = 32768 ifTrue: [^ (self pixelValueForDepth: 16) + 1].
	self error: 'unknown pixel depth'.
