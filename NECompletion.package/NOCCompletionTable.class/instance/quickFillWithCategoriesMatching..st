protocol
quickFillWithCategoriesMatching: pattern
	(Smalltalk organization categoriesMatching: pattern) do: [:category | 
		(Smalltalk organization classesInCategory: category) do: [:e | self quickFillWithClass: e]]