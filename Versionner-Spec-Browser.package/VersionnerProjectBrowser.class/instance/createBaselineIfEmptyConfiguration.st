actions
createBaselineIfEmptyConfiguration
	configuration versions ifEmpty: 
		[ | tempProject|
		tempProject := MTProject new
						name: configuration name configurationBaseName;
						configurationClass: configuration;
						yourself.
		tempProject devWorkflow createInitialDevelopment ]