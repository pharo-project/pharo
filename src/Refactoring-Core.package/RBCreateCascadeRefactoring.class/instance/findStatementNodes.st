preconditions
findStatementNodes
	"Find the sequence to be combined."

	statementNodes := OrderedCollection new.
	sequenceNode statements do: [ :each |
		(sequenceNode isLast: each)
			ifFalse: [ self addStatementNode: each ]
			ifTrue: [
				| current |
				current := each.
				[ current isReturn or: [ current isAssignment ] ]
					whileTrue: [ current := current value ].
				self addStatementNode: current ] ]