lookup
lookupVar: name inContext: aContext
	"I'll lookup a variable in a class. The context does not play any role in here because variables in classes are statically defined"

	^ self lookupVar: name