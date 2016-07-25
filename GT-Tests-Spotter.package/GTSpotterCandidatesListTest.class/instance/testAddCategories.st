as yet unclassified
testAddCategories
	|categories|
	categories := { GTSpotterA1 . GTSpotterA2 . GTSpotterA3 }.
	
	self addCategories: categories.
	self assert: (self hasCategories: categories)