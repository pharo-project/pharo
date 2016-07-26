enumerating
withAllPanesAndPresentationsDo: aBlock
	super withAllPanesAndPresentationsDo: aBlock.
	self cachedPresentation ifNotNil: [ :presentation | presentation withAllPanesAndPresentationsDo: aBlock ]