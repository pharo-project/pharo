validation
copyConfiguration: aConfigurationClass to: metacelloRepositoryString
	"Copy the current configuration mcz file to the given repository (i.e., http: //... or directory://, etc.)"

	<apiDocumentation>
	| project |
	project := aConfigurationClass project.
	project projectPackage
		copySpecTo:
			((project repositorySpec)
				description: metacelloRepositoryString;
				yourself)