instance creation
class: aClass recategorizedFrom: oldCategory to: newCategory
	^self new
			classRecategorized: aClass;
			oldCategory: oldCategory;
			newCategory: newCategory;
			yourself