private
genReification: aSymbol for: aLink
	| plugin |
	plugin := (plugins at: aSymbol ifAbsent: [self error: 'reification not supported for this entity']) entity: entity link: aLink.	
	 ^plugin perform: self selectorForNode