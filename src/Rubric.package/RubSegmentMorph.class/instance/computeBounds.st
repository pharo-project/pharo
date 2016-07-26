private
computeBounds
	vertices ifNil: [^ self].
	self changed.
	self releaseCachedState.
	bounds := self curveBounds expanded.
	self layoutChanged.
	self changed.
