*Metacello-PharoExtensions
packagesOfGroupNamed: aName

	^ (self version groups detect: [ :g | g name = aName ]) includes