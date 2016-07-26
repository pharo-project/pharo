initialize
reset
	workBuffer ifNil:[workBuffer := self class allocateOrRecycleBuffer: 10000].
	self primInitializeBuffer: workBuffer.
	self primSetAALevel: self aaLevel.
	self primSetOffset: destOffset.
	self primSetClipRect: clipRect.
	self primSetEdgeTransform: edgeTransform.
	self primSetColorTransform: colorTransform.
	forms := #().