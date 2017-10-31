hooks
afterCheck: aNode mappings: mappingDict
	^ (mappingDict at: '`@.before') noneSatisfy: #containsReturn