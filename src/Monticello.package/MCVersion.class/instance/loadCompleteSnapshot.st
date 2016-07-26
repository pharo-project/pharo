accessing
loadCompleteSnapshot
	|definitions|
	
	definitions := OrderedCollection withAll: self snapshot definitions.
	
	self dependencies 
		do: [ :each| definitions addAll: each resolve completeSnapshot definitions ]
		displayingProgress: [ :item| 'Loading dependencies from: ', item package name ].
	
	^ MCSnapshot fromDefinitions: definitions