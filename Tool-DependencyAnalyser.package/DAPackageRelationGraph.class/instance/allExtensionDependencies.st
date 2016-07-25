computing
allExtensionDependencies
	| aCollectionOfExtensionDependency |
	aCollectionOfExtensionDependency := OrderedCollection new.
	
	self seenPackages do: [ :each |
		each dependencies do: [ :dependency |
			aCollectionOfExtensionDependency addAll: dependency extensionDependencies.
			 ]
		 ].
	^ aCollectionOfExtensionDependency