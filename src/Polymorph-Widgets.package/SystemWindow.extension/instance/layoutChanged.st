*Polymorph-Widgets
layoutChanged
	"No need to propagate to the world.
	Fixed to always flush layout cache."

	(self owner isNil or: [self owner isWorldMorph not])
		ifTrue: [^super layoutChanged].
	fullBounds := nil.
	self layoutPolicy ifNotNil: [:layout | layout flushLayoutCache]