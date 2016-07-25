geometry
privateMoveBy: delta
	self
		handleBoundsChange: [ 
			self paragraph moveBy: delta truncated.
			super privateMoveBy: delta truncated ]