tests - operations on categories
testRenameCategoryChangeTheNameOfThePackage
	"test that when we rename a category, the RPackage corresponding is updated with this new name"
	
	|XPackage| 
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	
	Smalltalk organization renameCategory: 'XXXXX' toBe: 'YYYYY'.
	self assert: (XPackage name = 'YYYYY').