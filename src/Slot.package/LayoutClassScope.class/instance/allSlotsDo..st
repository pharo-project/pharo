enumerating
allSlotsDo: aBlock
	parentScope allSlotsDo: aBlock.
	self do: aBlock