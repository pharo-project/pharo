validation
validateConfiguration: configurationClass debug: debugList recurse: aBool
	"Check the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

	<apiDocumentation>
	^ MetacelloMCVersionValidator validateConfiguration: configurationClass debug: debugList recurse: aBool