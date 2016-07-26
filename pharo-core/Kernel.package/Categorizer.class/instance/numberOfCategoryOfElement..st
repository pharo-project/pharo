private
numberOfCategoryOfElement: element 
	"Answer the index of the category with which the argument, element, is 
	associated."

	| categoryIndex elementIndex |
	categoryIndex := 1.
	elementIndex := 0.
	[(elementIndex := elementIndex + 1) <= elementArray size]
		whileTrue: 
			["point to correct category"
			[elementIndex > (categoryStops at: categoryIndex)]
				whileTrue: [categoryIndex := categoryIndex + 1].
			"see if this is element"
			element = (elementArray at: elementIndex) ifTrue: [^categoryIndex]].
	^0