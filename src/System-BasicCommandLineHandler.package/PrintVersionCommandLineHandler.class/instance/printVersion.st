actions
printVersion
	self stdout 
		nextPutAll: '[version] ';
		print: SystemVersion current majorMinorVersion;
		nextPutAll: ' ';
		print: SystemVersion current asString;
		nextPutAll: OSPlatform current lineEnding.