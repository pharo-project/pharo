tests
testNewProjectWithInitialVersionInRepository
	|  projectName configClass |

	projectName := 'VersionnerTest'.
	project := MTProject newNamed: projectName withInitialVersion: '0.1'  inRepository: ''.
	
	self assert: (project configurationClass notNil) description: 'Project configuration class is nil.'.
	self shouldnt: [configClass := Smalltalk globals at: (project configurationClass name)]
		raise: KeyNotFound
		description: 'Cannot find the Configuration class in the image.'.
	self assert: project name equals: projectName.
	
	configClass removeFromSystem.
	