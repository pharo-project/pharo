origins
localDirectory 
	^ self class userLocalDirectory 
		ifNil: [ (self imageDirectory / self class defaultLocalDirectoryName) ensureCreateDirectory ]