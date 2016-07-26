requirements
nonEmpty
	^ nonEmpty ifNil: [nonEmpty := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1 with: self element]