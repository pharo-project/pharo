accessing
cHeaderExtractor
	"This is a double dispatch with the OSPlatform class. Each platform should be respoonsible of sending the correct message to the shared pool (methods under protocol 'platforms') based on the current platform information (like wether it is 32 or 64 bits), such as unix32Extractor, macOSX32Extractor, etc.
	Answers an instance of a FFICHeaderExtractor specially created for current platform."
	^ Smalltalk os cHeaderExtractorFor: self