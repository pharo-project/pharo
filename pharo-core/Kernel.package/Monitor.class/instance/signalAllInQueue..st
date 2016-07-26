private
signalAllInQueue: anOrderedCollection
	 queuesMutex critical: [
		anOrderedCollection removeAllSuchThat: [ :each |
				each signal.
				true ] ]