drawing
frameRectangle: aRectangle on: aCanvas
	"Draw each border in turn."
	
	|r|
	r := aRectangle.
	self borders do: [:b |
		b frameRectangle: r on: aCanvas.
		r := r insetBy: b width]