building projects
projectB
	| project allGroup smallGroup |
	
	project := MTProject 
					newNamed: 'B'
					withInitialVersion: '0.2'
					inRepository: 'http://smalltalkhub.com/mc/dummy/A/main'.
	(1 to: 4) do: [ :i |
		project addDependency: (MTPackage newNamed: 'PackageA' , i asString) ].
	
	project addDependency: (MTDependantProject newNamed: 'LibXYZ').

	smallGroup := MTGroup newNamed: 'SmallGroup'.
	project packages do: [ :package | smallGroup addDependency: package ].
	project addDependency: smallGroup.
	
	allGroup := MTGroup newNamed: 'All'.
	allGroup addDependency: smallGroup.
	project addDependency: allGroup.
	
	^ project