mocks
mockMethod: aSymbol class: className source: sourceString meta: aBoolean
	^ MCMethodDefinition
		className: className
		classIsMeta: aBoolean
		selector:  aSymbol
		category: 'as yet unclassified'
		timeStamp: ''
		source: sourceString