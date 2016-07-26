encoding
encode: pathString
	^ pathString convertToWithConverter: LanguageEnvironment defaultFileNameConverter.