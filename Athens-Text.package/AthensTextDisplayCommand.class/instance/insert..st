accessing
insert: aCommands

	| nn |
	nn := next.
	next := aCommands.
	next tail next: nn