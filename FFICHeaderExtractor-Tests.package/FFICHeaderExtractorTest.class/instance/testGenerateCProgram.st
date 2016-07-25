tests
testGenerateCProgram
	| extractor cProgram | 
	extractor := self testingSharedPool cHeaderExtractor.
	extractor generateCProgram.
	cProgram := extractor cProgramFilename asFileReference readStream contents.
	self assert: (cProgram includesSubstring: '#include <signal.h>').
	self assert: (cProgram includesSubstring: '#include <errno.h>').
	self assert: (cProgram includesSubstring: '#ifdef SIGKILL
		printDefinedConstant(file, SIGKILL);
	#else
		printUndefinedConstant(file, SIGKILL);
	#endif

	#ifdef SIGTERM
		printDefinedConstant(file, SIGTERM);
	#else
		printUndefinedConstant(file, SIGTERM);
	#endif

	#ifdef WHATEVER
		printDefinedConstant(file, WHATEVER);
	#else
		printUndefinedConstant(file, WHATEVER);
	#endif')
	
