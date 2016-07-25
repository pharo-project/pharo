pager managing
removePager
	self pager ifNotNil: [
		self removeMorph: self pager.
		pager := nil.
		self updateList.
		self resizerChanged].
