testing
isAnd: msgNode
	| stmts |
	stmts := msgNode arguments second body statements.
	^ stmts size = 1 and: [ stmts first value = false ]