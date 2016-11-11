accessing
buildPragma: aPragma
	currentPragma := aPragma.
	self perform: aPragma selector withArguments: aPragma arguments.
	^ nodeList