tests - operations on categories
testRenameCategoryUpdateTheOrganizer
	"test that when we rename a category, the organizer dictionary is update with this new name, so that we can access the package with this new name as key"

	|XPackage| 
	
	
	self addXCategory.
	
	XPackage := self organizer packageNamed: #XXXXX.
	Smalltalk organization renameCategory: 'XXXXX' toBe: 'YYYYY'.
	self assert: (self organizer packageNamed: 'YYYYY' asSymbol) = XPackage.
	self deny: (self organizer includesPackageNamed: #XXXXX).