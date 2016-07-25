private
setVertices: newVertices
	vertices := newVertices.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds