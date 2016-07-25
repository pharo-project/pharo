accessing
statements: stmtCollection 
	statements := stmtCollection.
	statements do: [:each | each parent: self]