Extension { #name : 'SystemNavigation' }

{ #category : '*Deprecated12' }
SystemNavigation >> allUnimplementedCalls [
	"call is not a good term here, these are selectors"
	self
		deprecated: 'Changed selector for coherency'
		transformWith: '`@receiver allUnimplementedCalls' -> '`@receiver allSentButNotImplementedSelectors'.
	^ self allSentButNotImplementedSelectors
]
