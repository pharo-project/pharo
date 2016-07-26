testing
isOr: msgNode
	| stmts |
	stmts := msgNode arguments first body statements.
	^ stmts size = 1 and: [ stmts first value = true ]