testing
isInitializer
	^ selector = #initialize and: [classIsMeta]
	