morph-drawing
drawOn: aCanvas
	"super drawOn: aCanvas."
	aCanvas
		translateBy: self globalBounds origin - self brickBounds origin
		during: [ :canvas | self onDraw: canvas ]