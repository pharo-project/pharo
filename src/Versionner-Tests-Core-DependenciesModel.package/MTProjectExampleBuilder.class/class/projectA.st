building projects
projectA
	| project allGroup smallGroup |
	
	project := MTProject 
					newNamed: 'A'
					withInitialVersion: '0.1'
					inRepository: 'http://smalltalkhub.com/mc/dummy/A/main'.
	(1 to: 5) do: [ :i |
		project addDependency: (MTPackage newNamed: 'PackageA' , i asString) ].
	
	project addDependency: (MTDependantProject newNamed: 'LibXYZ').

	smallGroup := MTGroup newNamed: 'SmallGroup'.
	project packages do: [ :package | smallGroup addDependency: package ].
	project addDependency: smallGroup.
	
	allGroup := MTGroup newNamed: 'All'.
	allGroup addDependency: smallGroup.
	allGroup addDependency: project requiredProjects first.
	project addDependency: allGroup.
	
	^ project