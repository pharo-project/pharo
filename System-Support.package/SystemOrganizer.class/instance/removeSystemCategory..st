remove
removeSystemCategory: category
	"remove all the classes and traits associated with the category"

	 (self orderedTraitsIn: category) , (self superclassOrder: category)
		reverseDo: [:each | each removeFromSystem].

	self removeCategory: category.
