primitives
warpBits
	"Move those pixels!"

	cellSize < 1 ifTrue: [ ^self error: 'cellSize must be >= 1' ].

	self warpBitsSmoothing: cellSize
		sourceMap: (sourceForm colormapIfNeededForDepth: 32).
