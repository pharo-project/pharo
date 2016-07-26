enumerating
withAllPanesAndPresentationsDo: aBlock
	aBlock value: self.
	self presentations withAllPanesAndPresentationsDo: aBlock