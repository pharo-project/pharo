announcement handling
behaviorAdded: aClassAddedAnnoucement

	^ (aClassAddedAnnoucement classAdded isTrait)
		ifTrue: [ self traitAdded: aClassAddedAnnoucement ]
		ifFalse: [ self classAdded: aClassAddedAnnoucement ]