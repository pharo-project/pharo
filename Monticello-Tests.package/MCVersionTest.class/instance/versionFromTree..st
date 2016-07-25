building
versionFromTree: sexpr
	^ (self dependencyFromTree: sexpr) resolve