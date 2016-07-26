announcement handling
methodAdded: aMethodAddedAnnoucement

	self addEvent: 
		(EpMethodAddition 
			method: aMethodAddedAnnoucement methodAffected)