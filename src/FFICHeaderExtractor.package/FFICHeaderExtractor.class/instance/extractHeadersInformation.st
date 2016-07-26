extracting
extractHeadersInformation
	"This method takes care of extracting all the constants values of a FFISharedPool from C header files. The way it does this is by first autogenerating a C program that will write down into an output text file, each constant and its value. Then such C program is compiled and run."
	| log |
	[
	self 
		generateCProgram;
		compileCProgram;
		runCProgram.
	] on: Error do: [ :ex | 
		self error: 'Error: ', ex printString. '. You may want to check ', self logFilename
	].
	log := self logFilename asFileReference readStream contents.
	log ifNotEmpty: [ 
		log inspect.
		self inform: 'There was a problem in the autogeneration. You may want to check ', self logFilename
		 ].
	
	
