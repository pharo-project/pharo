action
fillRectangle: aRectangle on: aCanvas
	"Fill the given rectangle on the given canvas with the receiver.
	Render from bottom to top."
	
	self fillStyles do: [:fs |
		fs fillRectangle: aRectangle on: aCanvas]