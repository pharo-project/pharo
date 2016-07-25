announcement handling
traitRemoved: aClassRemovedAnnoucement

	| aChange |

	aChange := EpTraitRemoval 
		trait: aClassRemovedAnnoucement classRemoved.
	self addEvent: aChange