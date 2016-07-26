transitions
fadeImageHorFine: otherImage at: topLeft
	"Display fadeImageHorFine: (Form fromDisplay: (10@10 extent: 300@300)) reverse at: 10@10"
	^ self fadeImage: otherImage at: topLeft indexAndMaskDo:
		[:i :mask |
		mask fill: (0@(i-1) extent: mask width@1) fillColor: Color black.
		mask fill: (0@(i-1+16) extent: mask width@1) fillColor: Color black.
		(i*2) <= mask width]