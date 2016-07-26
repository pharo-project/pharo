tests
testCategories
	| categories |
	
	categories := self organization categories.
	self assert: categories notEmpty.