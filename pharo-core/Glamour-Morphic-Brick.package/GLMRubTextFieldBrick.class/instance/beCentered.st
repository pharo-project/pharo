brick-properties
beCentered

	isCentered := true.
	self isVFilling
		ifTrue: [ self rubric forceHeight: [ self height - self vPadding ] ].