platforms
macOSX32Extractor
	"This is the default setup of a FFICHeaderExtractor for OSX 32 bits. Shared pool subclasses may need to override this if they want to add or change the default settings. For example, setting #includeFiles: to custom directories."
	^ FFICHeaderExtractor new
		sharedPool: self;
		platformName: 'Mac OS'; "IMPORTANT: Should match with 'Smalltalk os platformName ' "
		wordSize: 4;
		cFlags: '-m32';
		headersToInclude: self headersContainingVariables;
		compiler: 'cc';
		yourself