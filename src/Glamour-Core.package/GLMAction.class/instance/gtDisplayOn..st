printing
gtDisplayOn: stream
	self hasTitle ifTrue: [
		stream nextPutAll: self title; space ].
	self hasShortcut ifTrue: [
		self keymap gtDisplayOn: stream.
		stream space ].
	self action gtDisplayOn: stream