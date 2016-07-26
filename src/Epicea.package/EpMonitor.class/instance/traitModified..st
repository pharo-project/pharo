announcement handling
traitModified: aClassModifiedClassDefinitionAnnouncement

	| aChange |

	aChange := EpTraitModification
		oldTrait: aClassModifiedClassDefinitionAnnouncement oldClassDefinition
		newTrait: aClassModifiedClassDefinitionAnnouncement newClassDefinition.
	self addEvent: aChange