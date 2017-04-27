announcement handling
behaviorRemoved: aClassRemovedAnnoucement

	"Workaround: the announcement occurs after class is renamed to AnObsolete and removed from package."
	| classRemoved |
	classRemoved := aClassRemovedAnnoucement classRemoved asEpiceaRingDefinition.

	classRemoved definitionSource: 
		(classRemoved definitionSource
			copyReplaceAll: aClassRemovedAnnoucement classRemoved name printString
			with: aClassRemovedAnnoucement classRemoved originalName printString).
	classRemoved definitionSource: 
		(classRemoved definitionSource
			copyReplaceAll: 'Unclassified' printString
			with: aClassRemovedAnnoucement categoryName printString).

	classRemoved name: aClassRemovedAnnoucement classRemoved originalName.
	classRemoved category: aClassRemovedAnnoucement categoryName.
	classRemoved package: aClassRemovedAnnoucement packageAffected name.

	^ (aClassRemovedAnnoucement classAffected isTrait)
		ifTrue:  [ self traitRemoved: classRemoved ]
		ifFalse: [ self classRemoved: classRemoved ]