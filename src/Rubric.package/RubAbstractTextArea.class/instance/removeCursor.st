accessing cursor
removeCursor
	self cursor ifNotNil: [ :c | 
		self cursor aboutToBeRemoved.
		self removeMorph: c ].
	cursor := nil