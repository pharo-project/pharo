accessing
localRepository

	^ localRepository ifNil: [
		localRepository := MCDirectoryRepository new
			directory: directory asFileReference;
			yourself.	]