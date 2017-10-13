accessing
packagesOfGroupNamed: aName

	^ (self version groups detect: [ :g | g name = aName ]) includes