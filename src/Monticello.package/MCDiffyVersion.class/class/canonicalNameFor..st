name utilities
canonicalNameFor: aFileName
	^(self nameForVer: (self verNameFrom: aFileName)
		base: (self baseNameFrom: aFileName))
			, '.', MCMcdReader extension
