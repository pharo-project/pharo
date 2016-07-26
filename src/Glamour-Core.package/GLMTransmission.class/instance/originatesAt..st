testing
originatesAt: aPort
	^ (self activeOrigins includes: aPort) and: [self meetsCondition]