*Ring-Monticello
classDefinitionString
	"Answer a string describing the class-side definition."
	
	^String streamContents: [:stream | self printMetaDefinitionOn: stream]