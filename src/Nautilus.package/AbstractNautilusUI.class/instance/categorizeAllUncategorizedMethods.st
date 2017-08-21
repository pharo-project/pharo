menus behavior
categorizeAllUncategorizedMethods
	"Categorize methods by looking in parent classes for a method category.
	In addition, it automatically removed empty categories"
	self selectedClass ifNotNil: [ :aClass | | methods |
		methods := aClass uncategorizedSelectors collect: [ :selector| aClass >> selector ].
		self model classifier classifyAll: methods ].
	self removeEmptyCategories