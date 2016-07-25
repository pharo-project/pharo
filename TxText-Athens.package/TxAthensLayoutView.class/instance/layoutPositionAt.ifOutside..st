geometry
layoutPositionAt: aTextPosition ifOutside: aBlock
	"calculate the point at given text position, might return an invalid position (outside the layout)"
	^ self layout positionAt: aTextPosition ifOutside: aBlock