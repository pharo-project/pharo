testing
hasChanges
	|  initialProject |
	
	versionList selectedItem ifNil: [ ^ false ].
	initialProject := MTProject 
						newFromVersion: versionList selectedItem version
						inConfiguration: configuration configurationClass.
	^ (MTModelComparator is: project equalsTo: initialProject) not