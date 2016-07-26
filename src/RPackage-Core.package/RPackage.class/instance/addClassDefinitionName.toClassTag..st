class tags
addClassDefinitionName: aClassName toClassTag: aSymbol
	"Tags the class aClass with the tag aSymbol"
	
	(self addClassTag: aSymbol) addClassNamed: aClassName.