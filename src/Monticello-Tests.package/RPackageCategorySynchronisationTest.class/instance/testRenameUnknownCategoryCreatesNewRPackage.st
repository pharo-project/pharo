tests - operations on categories
testRenameUnknownCategoryCreatesNewRPackage
	"test that when we rename a category that is not registered in RPackage , it does not raise errors and simply create a new package. We need this behaviour as for now, create a new category with the class browser does not emit the corrects events, and therefore RPackage can not be directly updated"
	
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self addXCategory.
		].
	self deny: (self organizer includesPackageNamed: #XXXXX).
	Smalltalk organization renameCategory: 'XXXXX' toBe: 'YYYYY'.
	self assert: (self organizer includesPackageNamed: #YYYYY).