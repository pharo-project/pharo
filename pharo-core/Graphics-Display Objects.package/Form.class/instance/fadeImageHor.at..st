transitions
fadeImageHor: otherImage at: topLeft
	"Display fadeImageHor: (Form fromDisplay: (10@10 extent: 300@300)) reverse at: 10@10"
	^ self fadeImage: otherImage at: topLeft indexAndMaskDo:
		[:i :mask |
		mask fill: (0@(mask height//2-i) extent: mask width@(i*2)) fillColor: Color black.
		(i*2) <= mask width]