private-formatting
formatSequenceNodeStatementsFor: aSequenceNode
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty
		ifTrue: [ ^ self ].
	1 to: statements size do:
		[ :i | 
		self visitNode: (statements at: i).
		i < statements size
			ifTrue: [ codeStream nextPut: $.. 
						self newLine].
		self formatStatementCommentsFor: (statements at: i).
		i < statements size
			ifTrue: [  ] ]