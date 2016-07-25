accessing
colorForInsets
	"Return the color to be used for shading inset borders."
	
	self owner isSystemWindow
		ifTrue: [^self owner colorForInsets].
	^super colorForInsets