api-configuration
updateVersionMethodForVersion: inputVersionStringOrSymbol versionSpecsDo: aBlock
	^ self
		updateVersionMethodForVersion: inputVersionStringOrSymbol
		updateProjects: true
		updatePackages: true
		versionSpecsDo: aBlock