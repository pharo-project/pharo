scrollbars
verticesForSimpleArrow: aRectangle 
	"PRIVATE - answer a collection of vertices to draw a simple arrow"
	| vertices |
	vertices := OrderedCollection new.
	""
	vertices add: aRectangle bottomLeft.
	vertices add: aRectangle center x @ (aRectangle top + (aRectangle width / 8)).
	vertices add: aRectangle bottomRight.
	vertices add: aRectangle bottomRight + (0@0.01).
	""
	^ vertices