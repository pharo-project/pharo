querying
hasDecorator: aDecorator
	^ self = aDecorator or: [ next hasDecorator: aDecorator ]