accessing
requiredMethod
	| numberOfArguments binary |
	self isRequired ifFalse: [^nil].
	self size = 1 ifTrue: [^self locatedMethods anyOne].

	binary := self isBinarySelector.
	numberOfArguments := self numberOfArguments.
	
	 ^self
		generateMethod: self selector
		withMarker: CompiledMethod implicitRequirementMarker
		forArgs: numberOfArguments
		binary: binary.