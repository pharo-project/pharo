updating
update: aspect
	"A join has probably changed its selection state."

	super update: aspect.
	aspect == #selectedDifferences
		ifTrue: [self changed: #compositeText]