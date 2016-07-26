tests
testAddConfigurationCmd

	| versionString packageNames projectNames project version packages projects |
	self tearDownPackages addAll: #('ConfigurationOfMBFooTests' 'MBFooCommandTestA' 'MBFooCommandTestB'). "must be BEFORE the load"
	gofer version: 'MBFooCommandTestA-dkh.1'.
	gofer version: 'MBFooCommandTestB-dkh.1'.
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	versionString := '1.0'.
	packageNames := #('MBFooCommandTestA' 'MBFooCommandTestB').
	projectNames := #('ConfigurationOfMBFooTests').
	MBAddConfigurationCommand new
		executeAddBaselineForConfigurationNamed: self configurationName
		repositoryDescription: 'dictionary://', self repositoryName asString
		versionString: versionString
		packages: packageNames
		configurations: projectNames.
	project := (Smalltalk at: self configurationName asSymbol) project.
	version := project version: versionString, '-baseline'.
	packages := version packages.
	self assert: packages size == 2.
	packages do: [:pkg |
		self assert: (packageNames includes: pkg name)].
	projects := version projects.
	projects do: [:prj |
		self assert: (projectNames includes: prj name)].
