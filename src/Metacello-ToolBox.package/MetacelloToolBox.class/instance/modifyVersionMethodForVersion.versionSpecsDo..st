api-configuration
modifyVersionMethodForVersion: inputVersionStringOrSymbol versionSpecsDo: aBlock
	^ self
		updateVersionMethodForVersion: inputVersionStringOrSymbol
		updateProjects: false
		updatePackages: false
		versionSpecsDo: aBlock