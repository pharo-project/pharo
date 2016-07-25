adding
push: aPDPackage
	stack push: aPDPackage.
	aPDPackage inStack: true. "use a flag to avoid stack access"