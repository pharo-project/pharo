announcement handling
behaviorRemoved: aClassRemovedAnnoucement

	self handleAnyErrorDuring: [
		"Workaround: the announcement occurs after class is renamed to AnObsolete 
		and removed from package."
		
		| classRemoved |
		classRemoved := aClassRemovedAnnoucement classRemoved asEpiceaRingDefinition.

		"Fix class name (remove AnObsolete prefix)"
		classRemoved definitionSource: 
			(classRemoved definitionSource
				copyReplaceAll: aClassRemovedAnnoucement classRemoved name printString
				with: aClassRemovedAnnoucement classRemoved originalName printString).

		"Fix class name in the metaclass"
		classRemoved theMetaClass definitionSource: 
			(classRemoved theMetaClass definitionSource
				copyReplaceAll: aClassRemovedAnnoucement classRemoved name
				with: aClassRemovedAnnoucement classRemoved originalName).

		"Fix category"
		classRemoved definitionSource: 
			(classRemoved definitionSource
				copyReplaceAll: 'category: ', 'Unclassified' printString
				with: 'category: ', aClassRemovedAnnoucement categoryName printString).

		classRemoved name: aClassRemovedAnnoucement classRemoved originalName.
		classRemoved category: aClassRemovedAnnoucement categoryName.
		classRemoved package: aClassRemovedAnnoucement packageAffected name.

		aClassRemovedAnnoucement classAffected isTrait
			ifTrue:  [ self traitRemoved: classRemoved ]
			ifFalse: [ self classRemoved: classRemoved ] ]