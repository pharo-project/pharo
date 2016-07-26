properties
spotterExtMethods
	^ GTSpotter spotterExtendingMethods select: [ :m |
		m compiledMethod package = #QualityAssistant asPackage ]