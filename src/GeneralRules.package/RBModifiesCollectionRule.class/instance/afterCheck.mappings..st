initialization
afterCheck: aNode mappings: mappingDict
	^ self
		modifiesCollection: (mappingDict at: '`@collection')
		inAnyStatement: (mappingDict at: '`@.Statements')