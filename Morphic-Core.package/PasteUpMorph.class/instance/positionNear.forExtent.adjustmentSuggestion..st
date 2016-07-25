dropping/grabbing
positionNear: aPoint forExtent: anExtent adjustmentSuggestion: adjustmentPoint
	"Compute a plausible positioning for adding a subpart of size anExtent, somewhere near aPoint, using adjustmentPoint as the unit of adjustment"

	| adjustedPosition |
	adjustedPosition := aPoint.
	[((self morphsAt: (adjustedPosition + (anExtent // 2))) size > 1) and:  "that 1 is self here"
		[bounds containsPoint: adjustedPosition]]
	whileTrue:
		[adjustedPosition := adjustedPosition + adjustmentPoint].

	^ adjustedPosition