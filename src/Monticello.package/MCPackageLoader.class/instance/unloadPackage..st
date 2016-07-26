public
unloadPackage: aPackage
	self updatePackage: aPackage withSnapshot: MCSnapshot empty.
	MCMethodDefinition cachedDefinitions finalizeValues.