menu
makeOpen
	closed := false.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds