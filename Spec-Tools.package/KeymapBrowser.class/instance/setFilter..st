private
setFilter: aFilterText
	aFilterText isEmpty
		ifTrue: [ self kmTable resetFilteringBlock ]
		ifFalse: [ self kmTable
				filteringBlock: [ :items | items select: [ :item | 
						(self wrapItem: item) anySatisfy: [ :s | s includesSubstring: aFilterText caseSensitive: false ] ] ] ]