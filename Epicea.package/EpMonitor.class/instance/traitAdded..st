announcement handling
traitAdded: aClassAddedAnnoucement

	| aChange |

	aChange := EpTraitAddition
		trait: aClassAddedAnnoucement classAdded.

	self addEvent: aChange