announcement handling
classAdded: aClassAddedAnnouncement 

	(aClassAddedAnnouncement classAdded isKindOf: RBLintRule) 
		ifTrue: [ self reset ] 