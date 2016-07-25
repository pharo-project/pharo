development support
createNewDevelopmentVersionIn: aConfigurationClass description: creationMessage
	"Create a new development version using the #stable version as model."

	<apiDocumentation>
	| currentVersion developmentVersion |
	currentVersion := aConfigurationClass project version: #stable.
	developmentVersion := currentVersion versionNumber copy incrementMinorVersionNumber.
	self
		createDevelopment: developmentVersion versionString
		for: aConfigurationClass name asString
		from: currentVersion versionString
		imports: currentVersion importedVersions
		description: creationMessage