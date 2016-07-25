building projects
projectC
	| project |
	
	project := MTProject 
					newNamed: 'C'
					withInitialVersion: '0.1'
					inRepository: 'http://smalltalkhub.com/mc/dummy/C/main'.
	
	project addDependency: (MTDependantProject newNamed: 'LibXYZ').
	^ project