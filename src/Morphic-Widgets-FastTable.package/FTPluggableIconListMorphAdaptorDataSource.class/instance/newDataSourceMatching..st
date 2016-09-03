accessing
newDataSourceMatching: aFTFilter
	| newElements wrappedItem text newDataSource modelCopy |
	newElements := self elements select: [ :each | 
		wrappedItem := self elementWrapped: each.
		table columns anySatisfy: [ :column | 
			text := column transform: wrappedItem.
			aFTFilter matches: text]].
	
	newDataSource := self copy.
	modelCopy := self model copy.
	modelCopy prepareForFilteredDataSourceWith: newElements.
	newDataSource model: modelCopy.
	
	^newDataSource