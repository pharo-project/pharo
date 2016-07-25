scripts
createBranch: branchName for: configurationBasename description: commitMessage
	"Create a new branch for the project and save the configuration to it's repository."
	
	<apiDocumentation>
	| toolbox criticalIssues |
	branchName isEmpty ifTrue: [ ^self saveConfigurationPackageFor: configurationBasename description: commitMessage ].
	(criticalIssues := self checkForCriticalValidationIssues: configurationBasename) isEmpty
		ifTrue: [ 
			toolbox := self configurationNamed: configurationBasename.
			toolbox project goferBranch: branchName project: commitMessage.
			^toolbox ].
	 self error: 'There are critical issues in configuration ' , configurationBasename printString, '. Configuration not saved'.
	^nil