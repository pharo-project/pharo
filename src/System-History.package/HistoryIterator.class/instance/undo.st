undo-undo
undo
	self unplugWhile: [
		self current ifNotNil: [:curr | curr undo].
		self previous].
	^ true