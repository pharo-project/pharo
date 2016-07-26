notifying
notifyContextChangedFrom: aPreviousContext to: aCurrentContext
	self announce: (GTSpotterContextChanged new
		previous: aPreviousContext;
		current: aCurrentContext;
		yourself)