enumerating
withAllPanesAndPresentationsDo: aBlock
	super withAllPanesAndPresentationsDo: aBlock.
	self panes do: [:each | each withAllPanesAndPresentationsDo: aBlock ]