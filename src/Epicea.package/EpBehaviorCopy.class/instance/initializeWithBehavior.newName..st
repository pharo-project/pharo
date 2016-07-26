initialize
initializeWithBehavior: aClass newName: aName
	self initialize.
	
	definition := aClass asEpiceaRingDefinition.
	newName := aName asSymbol