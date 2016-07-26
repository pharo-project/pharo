transitions
fadeImageVert: otherImage at: topLeft
	"Display fadeImageVert: (Form fromDisplay: (10@10 extent: 300@300)) reverse at: 10@10"
	| d |
	d := self depth.
	^ self fadeImage: otherImage at: topLeft indexAndMaskDo:
		[:i :mask |
		mask fill: ((mask width//2//d-i*d)@0 extent: i*2*d@mask height) fillColor: Color black.
		i <= (mask width//d)]