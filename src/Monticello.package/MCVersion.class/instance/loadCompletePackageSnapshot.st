accessing
loadCompletePackageSnapshot
	| definitions |
	definitions := OrderedCollection withAll: package snapshot definitions.
	
	self dependencies 
		do: [ :each| definitions addAll: each package snapshot definitions ]
		displayingProgress: [ :item| 'Loading dependencies from: ', item package name ].
	
	^ MCSnapshot fromDefinitions: definitions
	
	