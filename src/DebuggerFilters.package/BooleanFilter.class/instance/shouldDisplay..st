testing
shouldDisplay: aContext
	^ (self filters first shouldDisplay: aContext) perform: booleanOperator with: [self filters last shouldDisplay: aContext]  