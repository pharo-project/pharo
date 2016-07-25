native
moduleName
	ModuleSession ~~ Smalltalk session ifTrue: [
		ModuleName := self findSDL2.
		ModuleSession := Smalltalk session
	].
	^ ModuleName