announcement handling
monticelloVersionSaved: aMCVersionSaved 

	aMCVersionSaved repository isEpiceaCache ifFalse: [
		self addEvent: aMCVersionSaved asEpiceaEvent]