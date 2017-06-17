announcement handling
behaviorModified: aClassModifiedClassDefinitionAnnouncement

	self handleAnyErrorDuring: [
		aClassModifiedClassDefinitionAnnouncement classAffected isTrait
			ifTrue: [ self traitModified: aClassModifiedClassDefinitionAnnouncement ]
			ifFalse: [ self classModified: aClassModifiedClassDefinitionAnnouncement ] ]