actions
cleanUpImage
	(self hasOption: 'release')
		ifTrue: [^ImageCleaner cleanUpForRelease].
	(self hasOption: 'production')	
		ifTrue: [ ^ImageCleaner cleanUpForProduction].
	Smalltalk cleanUp: true except: #() confirming: false.	