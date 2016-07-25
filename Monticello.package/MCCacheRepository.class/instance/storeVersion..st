storing
storeVersion: aVersion
	cacheEnabled == true ifFalse: [ ^ self ].
	^ super storeVersion: aVersion.