properties
spotterExtMethods
	^ GTSpotter spotterExtendingMethods select: [ :m |
		m compiledMethod package = #Renraku asPackage ]