*NautilusCommon
traitsDo: aBlock

	self systemDictionary allTraitsDo: [ :each | 
		(self includesClass: each)
			ifTrue: [ aBlock value: each ]  ]