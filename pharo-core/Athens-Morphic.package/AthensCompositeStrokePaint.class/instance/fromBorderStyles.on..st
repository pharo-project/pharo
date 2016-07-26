private
fromBorderStyles: borders on: anAthensCanvas
	strokePaints := borders collect: [:each | (each asAthensPaintOn: anAthensCanvas ) asStrokePaintOn: anAthensCanvas]