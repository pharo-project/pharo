text
classCommentString
	^ items
		detect: [ :ea | ea isClassDefinition and: [ ea className = classSelection ] ]
		ifFound: [ :classDefinition | classDefinition comment ]
		ifNone: [ '' ]