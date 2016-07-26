waiting-basic
waitUntil: aBlock
	"Conditional waiting for the default event.
	See Monitor>>waitWhile: aBlock."

	^ self waitUntil: aBlock for: nil