interface
versionWithInfo: aVersionInfo
	^self versionWithInfo: aVersionInfo ifNone: [ self error: 'Could not find version ', aVersionInfo name printString,'. Maybe you need to add a repository?' ]