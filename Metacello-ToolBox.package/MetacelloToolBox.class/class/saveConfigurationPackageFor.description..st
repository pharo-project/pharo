scripts
saveConfigurationPackageFor: configurationBasename description: commitMessage
	"Save mcz file that contains the configuration to it's repository."
	
	<apiDocumentation>
	| toolbox criticalIssues |
	(criticalIssues := self checkForCriticalValidationIssues: configurationBasename) isEmpty
		ifTrue: [ 
			toolbox := self configurationNamed: configurationBasename.
			toolbox project goferCommitProject: commitMessage.
			^toolbox ].
	 self error: 'There are critical issues in configuration ' , configurationBasename printString, '. Configuration not saved'.
	^nil