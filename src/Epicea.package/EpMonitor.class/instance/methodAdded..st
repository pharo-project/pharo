announcement handling
methodAdded: aMethodAddedAnnoucement

	self handleAnyErrorDuring: [
		self addEvent: (EpMethodAddition method: aMethodAddedAnnoucement methodAffected) ]