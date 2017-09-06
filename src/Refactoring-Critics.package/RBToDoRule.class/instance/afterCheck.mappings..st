hooks
afterCheck: aNode mappings: mappingDict
	^ self
		sendsAtWith: (mappingDict at: '`each')
		inAnyOf: (mappingDict at: '`@.Statements')