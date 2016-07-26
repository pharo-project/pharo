system compatibility
includesMethodCategory: categoryName ofClass: aClass
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClass: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]