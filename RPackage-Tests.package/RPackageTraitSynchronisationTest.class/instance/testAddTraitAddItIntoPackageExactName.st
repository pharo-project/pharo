tests - operations on traits
testAddTraitAddItIntoPackageExactName
	"self debug: #testAddTraitAddItIntoPackageExactName"

	|tmpPackage class|
	self addXCategory.
	self addXMatchCategory.

	tmpPackage := self organizer packageNamed: #XXXXX.
	class := self createNewTraitNamed: 'NewClass' inCategory: 'XXXXX'.

	self assert: (tmpPackage definesClass: class).
	self assert: tmpPackage equals: class package.