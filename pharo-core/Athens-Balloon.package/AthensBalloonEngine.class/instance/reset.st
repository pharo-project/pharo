initialize
reset
	workBuffer ifNil:[workBuffer := self class allocateOrRecycleBuffer: 10000].
	self primInitializeBuffer: workBuffer.
	self primSetAALevel: 4.
	self primSetOffset: 0@0.
	self primSetClipRect: clipRect.
	self primSetEdgeTransform: edgeTransform.
	self primSetColorTransform: colorTransform.
	forms := #().