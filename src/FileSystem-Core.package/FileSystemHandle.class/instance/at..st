public
at: index
	| buffer |
	buffer := ByteArray new: 1.
	self at: index read: buffer startingAt: 1 count: 1.
	^ buffer at: 1