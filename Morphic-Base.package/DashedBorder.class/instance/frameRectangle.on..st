drawing
frameRectangle: aRectangle on: aCanvas
	"Frame the given rectangle on aCanvas"
	
	(aRectangle width < self width or: [aRectangle height < self width])
		ifTrue: [^self]." don't do if too small"
	aCanvas
		frameRectangle: aRectangle
		width: self width
		colors: self dashColors
		dashes: self dashLengths