drawing
drawOn: aCanvas 
	"Draw in order:
	- background color
	- grid, if any
	- background sketch, if any
	Later (in drawSubmorphsOn:) I will skip drawing the background sketch."

	super drawOn: aCanvas.	
	self drawGridOn: aCanvas.
	self drawBackgroundSketchOn: aCanvas