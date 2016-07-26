accessing
buildPragma: aPragma
	currentPragma := aPragma.
	self perform: aPragma keyword withArguments: aPragma arguments.
	^ nodeList