announcement handling
behaviorModified: aClassModifiedClassDefinitionAnnouncement

	^ (aClassModifiedClassDefinitionAnnouncement classAffected isTrait)
		ifTrue: [ self traitModified: aClassModifiedClassDefinitionAnnouncement ]
		ifFalse: [ self classModified: aClassModifiedClassDefinitionAnnouncement ]