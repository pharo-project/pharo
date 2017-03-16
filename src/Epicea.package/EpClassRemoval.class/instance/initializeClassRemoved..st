initialization
initializeClassRemoved: aRemovedClass

	self initialize.
	classRemoved := aRemovedClass asEpiceaRingDefinition.

	self flag: #todo. "Workaround: the annoucement comes after class is renamed to AnObsolete..."
	classRemoved definitionSource: 
		(classRemoved definitionSource
			copyReplaceAll: aRemovedClass name
			with: aRemovedClass originalName).
	classRemoved name: aRemovedClass originalName.
