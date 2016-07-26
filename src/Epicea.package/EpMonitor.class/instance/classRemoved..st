announcement handling
classRemoved: aClassRemovedAnnoucement

	self addEvent: 
		(EpClassRemoval
			class: aClassRemovedAnnoucement classRemoved)