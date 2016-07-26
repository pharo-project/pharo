operations
addCategory: catString before: nextCategory
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."
	| index newCategory |
	newCategory := catString asSymbol.
	(categoryArray indexOf: newCategory) > 0
		ifTrue: [^self].	"heading already exists, so done"
	index := categoryArray indexOf: nextCategory
		ifAbsent: [categoryArray size + 1].
	categoryArray := categoryArray
		copyReplaceFrom: index
		to: index-1
		with: (Array with: newCategory).
	categoryStops := categoryStops
		copyReplaceFrom: index
		to: index-1
		with: (Array with: (index = 1
				ifTrue: [0]
				ifFalse: [categoryStops at: index-1])).
	"remove empty default category"
	(newCategory ~= Default
			and: [(self listAtCategoryNamed: Default) isEmpty])
		ifTrue: [self removeCategory: Default]