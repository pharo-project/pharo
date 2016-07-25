source code area
showScriptCode: anAnnouncement
	self setStylerClass: nil.
	self sourceTextModel setText: self sourceCode.
	self sourceTextModel clearUndoManager.
	self sourceTextModel hasUnacceptedEdits: false.