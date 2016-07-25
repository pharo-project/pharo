accessing
conflictMethod
	| binary numberOfArguments |
	self isConflict ifFalse: [^nil].
	binary := self isBinarySelector.
	numberOfArguments := self numberOfArguments.
	
	^self
		generateMethod: self selector
		withMarker: CompiledMethod conflictMarker
		forArgs: numberOfArguments
		binary: binary