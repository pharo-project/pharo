class tags
removeClassDefinition: aClass fromClassTag: aSymbol
	"Detags the class aClass with the tag aSymbol"
	
	self 
		removeClassDefinitionName: aClass name 
		fromClassTag: aSymbol