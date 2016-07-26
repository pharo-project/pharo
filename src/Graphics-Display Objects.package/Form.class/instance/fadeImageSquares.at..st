transitions
fadeImageSquares: otherImage at: topLeft 
	"Display fadeImageSquares: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40"
	^ self fadeImage: otherImage at: topLeft indexAndMaskDo:
		[:i :mask |
		mask fill: ((16-i) asPoint extent: (i*2) asPoint) fillColor: Color black.
		i <= 16]