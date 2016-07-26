private
loadMethods
	^ self model selectedCategories
		ifEmpty: [ self model methodsForCategory: self model allLabel ]
		ifNotEmpty: [ :cat | self model methodsForCategories: cat ]