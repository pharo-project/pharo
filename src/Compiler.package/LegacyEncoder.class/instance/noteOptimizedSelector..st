encoding
noteOptimizedSelector: aSymbol
	"Register a selector as being optimized.
	These optimized selectors will later be registered into the literals so that tools can easily browse senders."
	optimizedSelectors add: aSymbol