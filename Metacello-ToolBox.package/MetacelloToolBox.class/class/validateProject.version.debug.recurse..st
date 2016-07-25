validation
validateProject: aMetacelloProject version: versionString debug: debugList recurse: aBool
	"Check a specific version in the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

	<apiDocumentation>
	^ MetacelloMCVersionValidator
		validateProject: aMetacelloProject
		version: versionString
		debug: debugList
		recurse: aBool