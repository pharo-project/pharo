private
elementCategoryDict
	| dict firstIndex lastIndex |
	elementArray ifNil: [^ nil].
	dict := Dictionary new: elementArray size.
	1to: categoryStops size do: [:cat |
		firstIndex := self firstIndexOfCategoryNumber: cat.
		lastIndex := self lastIndexOfCategoryNumber: cat.
		firstIndex to: lastIndex do: [:el |
			dict at: (elementArray at: el) put: (categoryArray at: cat)].
	].
	^ dict.