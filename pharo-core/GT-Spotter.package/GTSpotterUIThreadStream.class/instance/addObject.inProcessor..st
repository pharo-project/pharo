adding/removing
addObject: firstObject inProcessor: secondObject

	self performSymbol: self onAddedSelector withArguments: { firstObject. secondObject }.
	Processor yield