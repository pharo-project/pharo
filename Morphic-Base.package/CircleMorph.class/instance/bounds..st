geometry
bounds: aRectangle
	| size |
	size := aRectangle width min: aRectangle height.
	super bounds: (Rectangle origin: aRectangle origin extent: size @ size).