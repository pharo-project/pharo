rotate scale and flex
prepareForScaling
	"If this morph requires a flex shell to scale,
	then wrap it in one and return it.
	Polygons, eg, may override to return themselves."

	^ self addFlexShell