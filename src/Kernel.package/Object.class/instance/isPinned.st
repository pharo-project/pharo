pinning
isPinned
	"self
		deprecated: 'Please use #isPinnedInMemory instead'
		transformWith: '`@receiver isPinned' -> '`@receiver isPinnedInMemory'."
		
	^self isPinnedInMemory