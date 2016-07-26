public query
defaultFileNameConverter
	FileNameConverter
		ifNil: [FileNameConverter := self currentPlatform class fileNameConverterClass new].
	^ FileNameConverter