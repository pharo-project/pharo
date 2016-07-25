instance creation
configurationClass: aClass reasonCode: aSymbol callSite: aCallSite explanation: aString

	^(self new)
		configurationClass: aClass;
		reasonCode: aSymbol;
		callSite: aCallSite;
		explanation: aString;
		yourself