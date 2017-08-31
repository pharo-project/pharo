announcement handling
categoryAdded: aCategoryAdded

	self handleAnyErrorDuring: [
		self addEvent: (EpCategoryAddition named: aCategoryAdded categoryName) ]