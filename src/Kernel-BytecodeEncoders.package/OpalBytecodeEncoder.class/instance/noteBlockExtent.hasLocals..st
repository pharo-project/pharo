temps
noteBlockExtent: blockExtent hasLocals: tempNodes
	blockExtentsToLocals ifNil:
		[blockExtentsToLocals := Dictionary new].
	blockExtentsToLocals at: blockExtent put: tempNodes asArray