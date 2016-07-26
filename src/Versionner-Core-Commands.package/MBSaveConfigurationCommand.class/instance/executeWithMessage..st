execute
executeWithMessage: message
	"execute with message"
	self checkUserInRepositoryOfPackage: self configurationClass name asString.
	MetacelloToolBox 
		saveConfigurationPackageFor: self configurationClass name asString 
		description: message