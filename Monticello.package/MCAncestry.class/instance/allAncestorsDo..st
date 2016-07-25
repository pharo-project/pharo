ancestry
allAncestorsDo: aBlock
	self ancestors do:
		[:ea |
		aBlock value: ea.
		ea allAncestorsDo: aBlock]