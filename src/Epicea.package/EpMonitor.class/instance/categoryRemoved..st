announcement handling
categoryRemoved: aPackageCategoryRemoved
	| aChange |
	aChange := EpCategoryRemoval named: aPackageCategoryRemoved categoryName.
	self addEvent: aChange.