drawing
debugDrawLineRectsOn: aCanvas
	"Shows where text line rectangles are"
	self paragraph lines do:
		[:line | aCanvas frameRectangle: line rectangle color: Color brown]
