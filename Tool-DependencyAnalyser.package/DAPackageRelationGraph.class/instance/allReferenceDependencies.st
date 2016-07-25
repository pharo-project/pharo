computing
allReferenceDependencies
	| aCollectionOfReferenceDependency |
	aCollectionOfReferenceDependency := OrderedCollection new.
	
	self seenPackages do: [ :each |
		each dependencies do: [ :dependency |
			aCollectionOfReferenceDependency addAll: dependency referenceDependencies.
			 ]
		 ].
	^ aCollectionOfReferenceDependency