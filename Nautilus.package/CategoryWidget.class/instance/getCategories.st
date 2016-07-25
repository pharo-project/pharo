protocol
getCategories

	^ categories ifNil: [ categories := self loadCategories ].