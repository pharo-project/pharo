testing
isDefaultFor: aPlatform 
	^  aPlatform isUnix 
		or: [ aPlatform isMacOSX 
		or: [ aPlatform isMacOS ] ]