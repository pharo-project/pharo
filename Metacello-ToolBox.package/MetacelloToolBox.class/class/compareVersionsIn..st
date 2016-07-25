development support
compareVersionsIn: aConfigurationClass
	"Compare the current #stable version to current #development version"

	<apiDocumentation>
	| developmentVersion previousVersion |
	developmentVersion := aConfigurationClass project version: #development.
	previousVersion := aConfigurationClass project version: #stable.
	^self
		compareVersion: previousVersion versionString
		for: aConfigurationClass name asString
		to: developmentVersion versionString
	