utility
disableCacheDuring: aBlock
	self disable.
	aBlock ensure: [ self enable ].