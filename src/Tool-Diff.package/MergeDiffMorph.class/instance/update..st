actions
update: aspect
	"A join has probably changed its selection state."

	super update: aspect.
	aspect == #joinClicked
		ifTrue: [self
				changed;
				changed: #selectedDifferences]