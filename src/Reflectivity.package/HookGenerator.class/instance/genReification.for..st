private
genReification: aSymbolOrNode for: aLink
	| plugin | 
	aSymbolOrNode isSymbol not ifTrue: [ ^ aSymbolOrNode]. 
	plugin := (plugins at: aSymbolOrNode ifAbsent: [self error: 'reification not supported for this entity']) entity: entity link: aLink.	
	 ^plugin perform: self selectorForNode