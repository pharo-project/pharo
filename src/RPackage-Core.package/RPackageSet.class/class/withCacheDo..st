accessing
withCacheDo: aBlock 
	| oldActive |
	
	oldActive := self isCacheActive.
	self cacheActive: true.
	aBlock ensure: [ 
		self cacheActive: oldActive.
		self isCacheActive ifFalse: [ self resetCache ] ].