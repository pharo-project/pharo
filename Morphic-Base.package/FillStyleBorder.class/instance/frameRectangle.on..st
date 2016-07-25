drawing
frameRectangle: aRectangle on: aCanvas
	"Fill the border areas with the fill style, clipping for each segment."
	
	(self borderRectsFor: aRectangle) do: [:r |
		aCanvas
			fillRectangle: r
			fillStyle: self fillStyle]