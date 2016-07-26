protocol
selectedClass
	^ self selectedClasses ifEmpty:[ nil]  ifNotEmpty:[: col | col first]