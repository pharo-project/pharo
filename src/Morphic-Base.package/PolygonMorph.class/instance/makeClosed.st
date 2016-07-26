menu
makeClosed
	closed := true.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds