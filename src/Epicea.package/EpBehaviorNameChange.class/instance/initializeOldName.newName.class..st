initialization
initializeOldName: anOldName newName: aNewName class: aBehavior

	self initialize.
	behavior := aBehavior asEpiceaRingDefinition.
	oldName := anOldName.
	newName := aNewName.
	