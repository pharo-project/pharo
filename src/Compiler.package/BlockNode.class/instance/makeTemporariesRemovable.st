code generation (closures)
makeTemporariesRemovable
	"Utilities for when we want to remove some temporaries."
	
	temporaries isArray ifTrue:
		[temporaries := temporaries asOrderedCollection].