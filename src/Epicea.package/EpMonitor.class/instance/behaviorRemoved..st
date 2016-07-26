announcement handling
behaviorRemoved: aClassRemovedAnnoucement

	^ (aClassRemovedAnnoucement classAffected isTrait)
		ifTrue:  [ self traitRemoved: aClassRemovedAnnoucement ]
		ifFalse: [ self classRemoved: aClassRemovedAnnoucement ]