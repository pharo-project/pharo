operation
insertLineSeparator: separator at: position
	| pos |
	pos := next startPosition.
	self insertNext: separator.
	^ pos
