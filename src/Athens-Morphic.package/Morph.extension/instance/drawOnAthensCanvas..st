*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas 
	| border |
	border := self borderStyle. 
	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas drawShape: self bounds.
	
	"Fill the given rectangle."
	anAthensCanvas drawShape: (self bounds insetBy: border width).

	(anAthensCanvas setStrokePaint: border).
	anAthensCanvas drawShape: (self bounds insetBy: (border width /2 asFloat)).
	

	"aBorderStyle frameRectangle: aRectangle on: self "
	
	