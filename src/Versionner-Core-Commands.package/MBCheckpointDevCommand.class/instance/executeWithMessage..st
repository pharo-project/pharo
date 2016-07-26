execute
executeWithMessage: message
	| configClass |
	configClass := self configurationClass.
	self checkUserInRepositoryOfPackage: configClass name asString.
	MetacelloToolBox saveModifiedPackagesAndConfigurationIn: configClass description: message.
