*Morphic-Base-keymapping
handleKeystrokeWithKeymappings: aKeystrokeEvent

	self allowsKeymapping ifTrue: [
		self dispatchKeystrokeForEvent: aKeystrokeEvent.
	].