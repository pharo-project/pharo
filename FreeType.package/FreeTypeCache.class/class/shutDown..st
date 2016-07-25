system startup
shutDown: isImageQuitting 
	isImageQuitting ifFalse: [ ^ self ].
	(current notNil and: [ self clearCacheOnShutdown ])
		ifTrue:[ 
			self current removeAll.
			current := nil ]