searching
lookUpForRemovedPackages
	| old |
	old := oldRelationGraph seenPackages reject: [ :each |  newRelationGraph includesSeenPackage: each ].	
	old do: [ :each |
		self addRemovedPackage: each
		 ]