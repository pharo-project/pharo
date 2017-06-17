announcement handling
categoryRenamed: aCategoryRenamed

	self handleAnyErrorDuring: [
		self addEvent: (EpCategoryRename
			oldName: aCategoryRenamed oldCategoryName
			newName: aCategoryRenamed newCategoryName) ]