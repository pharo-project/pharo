accessing-defaults
defaultCache
	^ IdentityDictionary new 
		at: #processorsAndFilters put: (IdentityDictionary new: 100); " not a limitation, just a reasonable start size "
		at: #processorsAndStreamed put: (IdentityDictionary new: 100); " not a limitation, just a reasonable start size "
		yourself