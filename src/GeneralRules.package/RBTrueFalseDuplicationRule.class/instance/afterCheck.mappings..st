hooks
afterCheck: aNode mappings: mappingDict
	
	mappingDict
		at: '`.LastStatement'
		ifPresent: [ :statement |
			^ statement isVariable not ].
	
	"the first statement is duplicated"
	^ true