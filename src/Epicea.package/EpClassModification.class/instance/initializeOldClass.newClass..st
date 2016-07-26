initialization
initializeOldClass: anOldClass newClass: aNewClass

	self initialize.
	oldClass := anOldClass asEpiceaRingDefinition theNonMetaClass.
	newClass := aNewClass asEpiceaRingDefinition theNonMetaClass.