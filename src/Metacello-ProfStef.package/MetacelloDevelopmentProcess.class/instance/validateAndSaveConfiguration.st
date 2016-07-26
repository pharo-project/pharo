lessons
validateAndSaveConfiguration
	^Lesson
		title: '3. Validate and save configuration in project repository'
		lesson: '"Before saving a configuration into the project repository, the configuration should be validated and any Critical Issues should be addressed:"
		
	(MetacelloToolBox validateConfiguration: ConfigurationOfExample) explore.

"Use Gofer to save the configuration to your project repository:"

	Gofer new
		url: ''http://www.example.com/ExampleRepository'';
		package: ''ConfigurationOfExample'';
		commit: ''Initial configuration''.

"Once you''ve done your initial commit, you can use the following expression to save the configuration to your project repository:"

	MetacelloToolBox saveConfigurationPackageFor: ''Example'' description: ''commit message''.
	
"Note that MetacelloToolBox class>>saveConfigurationPackageFor:description: validates the configuration before saving."

ProfStef next.
'