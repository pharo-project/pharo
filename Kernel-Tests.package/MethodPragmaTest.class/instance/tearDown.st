running
tearDown
	(self class organization listAtCategoryNamed: self methodCategory)
		do: [ :each | self class removeSelectorSilently: each ].
	self class organization removeCategory: self methodCategory.