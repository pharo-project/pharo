visiting
visitConfiguration: aConfigurationClass withVersionString: aVersionString
	"visits a Configuration class for the specified version and returns the project root object.
	 Returns the project root object or nil if the given version is nil."

	project := MTProject  new.
	project configurationClass: aConfigurationClass.
	self visitMCProject: aConfigurationClass project withVersion: (aConfigurationClass project version: aVersionString).
	^project
	