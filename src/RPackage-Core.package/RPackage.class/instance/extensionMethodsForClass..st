accessing
extensionMethodsForClass: aClass
	"Change the set of extensions selectors to an Array to avoid compiled methods collisions in the resulting set."

	^ (self extensionSelectorsForClass: aClass) asArray collect: [ :each | aClass >> each ]