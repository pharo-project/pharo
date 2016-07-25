container protocol
bottom
	"Note we should really check for contiguous pixels here"
	^ (self vertProfile findLast: [:count | count >= minWidth])
		+ shadowForm offset y