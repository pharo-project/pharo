testing
isIfNotNil: msgNode
	| stmts |
	stmts := msgNode arguments first body statements.
	^ stmts size = 1 and: [ stmts first = msgNode receiver ]