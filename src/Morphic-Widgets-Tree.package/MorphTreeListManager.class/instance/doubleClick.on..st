mouse managing
doubleClick: anEvent on: aMorph
	self doubleClickSelector  ifNil: [^false].
	client model perform: self doubleClickSelector withEnoughArguments: { anEvent. aMorph }.
	^ true