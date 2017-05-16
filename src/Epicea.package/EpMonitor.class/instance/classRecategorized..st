announcement handling
classRecategorized: aClassRecategorizedAnnoucement

	self handleAnyErrorDuring: [
		aClassRecategorizedAnnoucement newCategory = aClassRecategorizedAnnoucement oldCategory
			ifTrue: [ ^ self ].
			
		self addEvent: (EpBehaviorCategoryChange
			oldCategory: aClassRecategorizedAnnoucement oldCategory
			newCategory: aClassRecategorizedAnnoucement newCategory
			class: aClassRecategorizedAnnoucement classAffected)]