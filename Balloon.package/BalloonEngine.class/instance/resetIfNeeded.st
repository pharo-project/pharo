initialize
resetIfNeeded
	workBuffer ifNil:[self reset].
	self primSetEdgeTransform: edgeTransform.
	self primSetColorTransform: colorTransform.
	self primSetDepth: self primGetDepth + 1.
	postFlushNeeded := false.