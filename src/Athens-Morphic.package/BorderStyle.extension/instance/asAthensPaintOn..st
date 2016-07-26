*Athens-Morphic
asAthensPaintOn: anAthensCanvas
	| stroke |
	stroke := self color asStrokePaintOn: anAthensCanvas.
	stroke width: self width.
	stroke joinMiter.
	^ stroke