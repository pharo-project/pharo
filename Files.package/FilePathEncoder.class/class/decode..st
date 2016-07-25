encoding
decode: aString
	^ aString convertFromWithConverter: LanguageEnvironment defaultFileNameConverter.