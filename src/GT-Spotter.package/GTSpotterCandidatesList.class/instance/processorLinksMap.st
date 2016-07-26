accessing
processorLinksMap

	^ processorLinksMap ifNil: [ processorLinksMap := Dictionary new ]
	