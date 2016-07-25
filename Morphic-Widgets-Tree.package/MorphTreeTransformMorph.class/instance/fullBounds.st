geometry
fullBounds
	"Overridden to clip submorph hit detection to my bounds."
	"It might be better to override doLayoutIn:, and remove this method"

	fullBounds ifNotNil:[^ fullBounds].
	fullBounds := bounds.
	^ fullBounds