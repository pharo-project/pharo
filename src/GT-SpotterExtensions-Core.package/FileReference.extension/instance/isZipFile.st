*GT-SpotterExtensions-Core
isZipFile
	^ self isFile and: [ #('zip' 'jar' 'ear' 'war') includes: self extension ]