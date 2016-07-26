validation
validateConfiguration: configurationClass
	"Check the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

	<apiDocumentation>
	^ self validateConfiguration: configurationClass debug: #() recurse: false