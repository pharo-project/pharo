testing
includesGlobalNamed: aString 
	
	"It is a global if we have it in our system definition"
	^ systemDefinition includesClassNamed: aString.