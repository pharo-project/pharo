accessing
doItContext
	^ interactionModel ifNotNil: [ :im | im doItContext ]