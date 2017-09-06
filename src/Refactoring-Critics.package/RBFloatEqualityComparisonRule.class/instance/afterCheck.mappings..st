hooks
afterCheck: aNode mappings: mappingDict
	^ (mappingDict at: '`#floatLiteral') value isFloat