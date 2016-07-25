private
checkRequiredProjects: project
	| requiredProjects proj |
	
	requiredProjects := project requiredProjects.
	
	proj := requiredProjects at: 1.
	self assert: proj notNil.
	self assert:  proj displayName equals: 'BitmapCharacterSet'.
	self assert:  proj name equals: 'VersionnerTestBitmapCharacterSet'.
	self assert: proj version name equals: 'stable'.
	
	proj := requiredProjects at: 2.
	self assert: proj notNil.
	self assert: proj displayName equals: 'XMLWriter'.
	self assert: proj name equals: 'VersionnerTestXMLWriter'.
	self assert: proj version name equals: 'stable'.
