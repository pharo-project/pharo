tests - adding classes
testAddClassAddItIntoPackageExactName
	"self debug: #testAddCategoryCreatesPackage"

	|tmpPackage class|
	self addXCategory.
	self addXMatchCategory.

	tmpPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self assert: (tmpPackage definesClass: class).
	self assert: class package equals: tmpPackage