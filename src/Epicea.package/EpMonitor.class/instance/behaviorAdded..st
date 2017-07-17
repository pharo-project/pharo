announcement handling
behaviorAdded: aClassAddedAnnoucement

	self handleAnyErrorDuring: [
		aClassAddedAnnoucement classAdded isTrait
			ifTrue: [ self traitAdded: aClassAddedAnnoucement ]
			ifFalse: [ self classAdded: aClassAddedAnnoucement ] ]