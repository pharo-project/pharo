accessing
userLocalDirectory
	^ UserLocalDirectory ifNil: [ 
		FileLocator imageDirectory / self defaultLocalDirectoryName  ]
