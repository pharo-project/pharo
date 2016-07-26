geometry
transformedBy: aTransform
	self changed.
	self transform: (self transform composedWithGlobal: aTransform).
	self computeBounds.
	self changed.