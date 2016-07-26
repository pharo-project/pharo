settings
usedStrategy
	^ UsedStrategy ifNil: [UsedStrategy := #staggerFor:initialExtent:world:]