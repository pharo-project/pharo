accessing
allMethods

	^ self protocols flatCollect: [:p | p methods ].