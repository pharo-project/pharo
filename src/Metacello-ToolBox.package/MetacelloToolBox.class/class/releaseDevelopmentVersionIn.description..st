development support
releaseDevelopmentVersionIn: aConfigurationClass description: commitMessage
	"Release #development version: set version blessing to #release, update the #development and #stable symbolic version methods and save the configuration."

	<apiDocumentation>
	| version |
	version := aConfigurationClass project version: #development.
	self
		releaseVersion: version versionString for: aConfigurationClass name asString;
		saveConfigurationPackageFor: aConfigurationClass name asString description: commitMessage