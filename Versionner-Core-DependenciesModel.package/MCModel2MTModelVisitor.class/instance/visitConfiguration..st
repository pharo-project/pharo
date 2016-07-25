visiting
visitConfiguration: aConfigurationClass
	"Visits a Configuration class with the current version."

	| currentVersion |
	
	currentVersion := aConfigurationClass project currentVersion.
	^ self visitConfiguration: aConfigurationClass withVersion: currentVersion.
