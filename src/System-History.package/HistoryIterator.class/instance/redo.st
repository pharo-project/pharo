undo-undo
redo
	self unplugWhile: [
		self hasNext ifFalse: [^false].
		self next redo].
	^ true