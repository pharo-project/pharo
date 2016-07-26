accessing instances and variables
includesSharedPoolNamed:  aSharedPoolString 
	"Answer whether the receiver uses the shared pool named aSharedPoolString"
	
	^ (self sharedPools anySatisfy: [:each | each name = aSharedPoolString])