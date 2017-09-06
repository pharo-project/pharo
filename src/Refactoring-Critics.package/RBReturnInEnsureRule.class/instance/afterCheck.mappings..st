hooks
afterCheck: aNode mappings: mappingDict
	^ (mappingDict at: '`@.Stmts')
		anySatisfy: #containsReturn