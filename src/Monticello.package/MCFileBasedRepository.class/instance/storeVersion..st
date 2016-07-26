storing
storeVersion: aVersion

	(self includesFileNamed: aVersion fileName) ifTrue: [ ^ self ].
	^ super storeVersion: aVersion.