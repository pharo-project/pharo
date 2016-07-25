t-rotating
prepareForRotating
	"If I require a flex shell to rotate,
	then wrap it in one and return it.
	Polygons, eg, may override to do nothing."

	^ self addFlexShell