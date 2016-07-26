accessing
removeCategory: category
	| r |
	r := super removeCategory: category.
	
	"If the category we try to remove is the default one, due to cleansing, we don't have to announce it since it is not a valid category for a Class"
	category = Default ifFalse: [
		SystemAnnouncer uniqueInstance classCategoryRemoved: category.
	].

	^ r