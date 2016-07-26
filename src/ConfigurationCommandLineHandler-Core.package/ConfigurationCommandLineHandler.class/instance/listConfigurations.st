actions
listConfigurations
	"List possible configurations of the given repository"
	
	| configurations |
	
	self inform: 'All Configurations found in ', self repositoryUrl.
	
	configurations := self loadConfigurationNames.
	configurations ifEmpty: [ ^ self errorNoConfigurationsFound ].
	self printConfigurations: configurations.
	
	^ self exitSuccess