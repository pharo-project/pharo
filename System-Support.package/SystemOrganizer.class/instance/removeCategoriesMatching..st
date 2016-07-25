remove
removeCategoriesMatching: matchString
	"Remove all matching categories with their classes"
	(self categoriesMatching: matchString) do:
		[:c | self removeSystemCategory: c]