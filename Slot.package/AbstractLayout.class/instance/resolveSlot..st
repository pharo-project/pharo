accessing
resolveSlot: aName
	self flag: 'Signal rather than error'.
	self error: 'No slots found'