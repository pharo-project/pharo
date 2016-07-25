visiting
visitConfiguration: aConfigurationClass withVersion: aVersion
	"visits a Configuration class for the specified version and returns the project root object.
	 Returns the project root object or nil if the given version is nil."

	aVersion ifNil: [ ^ nil ].
	project := MTProject  new.
	project configurationClass: aConfigurationClass.
	self visitMCProject: aConfigurationClass project withVersion: aVersion.
	^project
	