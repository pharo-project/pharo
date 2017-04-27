popup - opening
beRightToLeftWithOffset: anOffset

	self invalidateWith: [
		self triggerBrick hAlign: #right.
		self triggerBrick marginRight: anOffset.
		self foundationBrick bounds: (self foundationBrick bounds translateBy: (self width - self triggerBrick width - anOffset - inset left @ 0) negated) ]