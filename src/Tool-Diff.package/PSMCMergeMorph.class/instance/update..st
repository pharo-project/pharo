updating
update: aspect
	"A join has probably changed its selection state."

	super update: aspect.
	aspect == #compositeText
		ifTrue: [self updateCode].
	aspect == #changes
		ifTrue: [self
				changed: #conflictCountString;
				changed: #allConflictsResolved;
				changed: #notAllConflictsResolved]