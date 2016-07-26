enumerating
allSlotsReverseDo: aBlock
	self reverseDo: aBlock.
	parentScope allSlotsReverseDo: aBlock.