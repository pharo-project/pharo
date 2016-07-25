generating C program
writeListOfConstantsOn: aStream
	"Internal method for the C program creation. This one iterates each class variable of the shared pool and calls the correct function to write the values into the output file."
	self sharedPool classVarNames withIndexDo: [ :each :index |
		aStream nextPutAll: (('
	#ifdef <1s>
		printDefinedConstant(file, <1s>);
	#else
		printUndefinedConstant(file, <1s>);
	#endif') expandMacrosWith: each); cr
	].
	