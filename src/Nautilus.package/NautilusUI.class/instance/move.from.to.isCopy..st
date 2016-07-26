drag and drop
move: item from: oldCategory to: newCategory isCopy: isCopy

	| category |
	category := newCategory.
	category = self allLabel
		ifTrue: [ category := self asYetUnclassifiedString ].
	isCopy
		ifFalse: [
			item methodClass organization
				classify: item selector
				under: category 
				suppressIfDefault: true.
			self selectedMethod: item ] 