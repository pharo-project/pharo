lookup
lookupVarForDeclaration: name
	"subclass responsibility"

	^ self outerScope lookupVarForDeclaration: name