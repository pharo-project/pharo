testing
isActivePlatform
	^ self isMacOS and: [ self currentVersion asNumber >= 1000 ]