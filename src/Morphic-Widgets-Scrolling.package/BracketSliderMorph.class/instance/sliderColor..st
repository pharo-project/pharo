access
sliderColor: newColor
	"Set the slider colour."
	
	super sliderColor: (self enabled ifTrue: [Color black] ifFalse: [self sliderShadowColor]).
	slider ifNotNil: [slider borderStyle baseColor: Color white]