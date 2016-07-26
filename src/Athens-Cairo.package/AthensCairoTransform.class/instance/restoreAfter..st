transformations
restoreAfter: aBlock
	self save.
	aBlock ensure:  [ self restore ].