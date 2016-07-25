validation
validateProject: aMetacelloProject version: versionString
	"Check a specific version in the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

	<apiDocumentation>
	^ self validateProject: aMetacelloProject version: versionString debug: #() recurse: false
