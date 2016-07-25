*Monticello-RPackage
isDirty
	
	| mcp |
	
	mcp := MCPackage named: self name.

	^(MCWorkingCopy registry 
		at: mcp
		ifPresent: [ :item | item modified ]
		ifAbsent: [ false ])