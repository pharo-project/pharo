enumerating
withAllPanesAndPresentationsDo: aBlock
	super withAllPanesAndPresentationsDo: aBlock.
	self do: [:each | each withAllPanesAndPresentationsDo: aBlock ]