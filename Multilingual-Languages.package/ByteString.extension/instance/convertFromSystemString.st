*Multilingual-Languages
convertFromSystemString

	| readStream |
	readStream := self readStream.
	^ self class new: self size streamContents: [ :writeStream |
		| converter |
		converter := LanguageEnvironment defaultSystemConverter.
		[readStream atEnd] whileFalse: [
			writeStream nextPut: (converter nextFromStream: readStream)]].
