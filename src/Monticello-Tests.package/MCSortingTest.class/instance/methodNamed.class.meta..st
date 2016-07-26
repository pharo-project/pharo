building
methodNamed: aSymbol class: className meta: aBoolean
	^ MCMethodDefinition
		className: className
		classIsMeta: aBoolean
		selector: aSymbol
		category: ''
		timeStamp: ''
		source: ''