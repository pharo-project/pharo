instance creation
basicFromUrl: aZnUrl
	^ self new directory: (self urlAsFileReference: aZnUrl)