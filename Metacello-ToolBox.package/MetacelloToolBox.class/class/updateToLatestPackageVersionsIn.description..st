development support
updateToLatestPackageVersionsIn: aConfigurationClass description: descriptionString
	"Update the #development version to match currently loaded mcz files."

	"self updateToLatestPackageVersions: '- fixed a bug'"

	<apiDocumentation>
	^self
		updateDevelopment: #development
		for: aConfigurationClass name asString
		updateProjects: false 
		description: descriptionString