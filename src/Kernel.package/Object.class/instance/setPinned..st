pinning
setPinned: aBoolean
	"self
		deprecated: 'Please use #setPinnedInMemory: instead'
		transformWith: '`@receiver setPinned: `@statements' -> '`@receiver setPinnedInMemory: `@statements'."
		
	self setPinnedInMemory: aBoolean