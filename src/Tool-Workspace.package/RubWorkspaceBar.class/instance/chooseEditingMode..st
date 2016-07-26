managing editing mode
chooseEditingMode: anEvent
	self modeListMenu invokeAt: anEvent position in: self world.
	self updateContents