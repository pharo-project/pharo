composition
- anArrayOfSelectors
	^TraitExclusion
		with: self
		exclusions: (anArrayOfSelectors isSymbol 
							ifTrue: [{ anArrayOfSelectors }] 
							ifFalse: [ anArrayOfSelectors])