running
computeHubsFor: aNode

	aNode hub: (aNode nextNodes inject: 0 into: [:s :n| s + n auth])