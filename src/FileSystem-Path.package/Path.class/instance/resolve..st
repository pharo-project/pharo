navigating
resolve: anObject
	"Return a path in which the argument has been interpreted in the context of the receiver. Different 
	argument types have different resolution semantics, so we use double dispatch to resolve them correctly."
	
	^ anObject asResolvedBy: self