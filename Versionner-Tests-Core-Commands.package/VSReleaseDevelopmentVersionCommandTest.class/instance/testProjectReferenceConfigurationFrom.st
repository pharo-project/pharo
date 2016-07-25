tests
testProjectReferenceConfigurationFrom
	| config |
	
	config := cmd 
		projectReference: 'XMLWriter'
		configurationFrom: project devWorkflow developmentVersion.
	self 
		assert: config class
		equals: MetacelloMCProject.
	self 
		assert: config configuration class
		equals: ConfigurationOfVersionnerTestXMLWriter