initialization
initializeWithOldMethod: anOldMethod newMethod: aNewMethod

	self initialize.
	oldMethod := anOldMethod asEpiceaRingDefinition.
	newMethod := aNewMethod asEpiceaRingDefinition.