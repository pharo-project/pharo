lookup
lookupVar: name
	"subclass responsibility"

	^ self outerScope lookupVar: name