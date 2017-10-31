tests - operations on traits
testAddTraitAddItIntoPackageBestMatchName
	"self debug: #testAddTraitAddItIntoPackageBestMatchName"

	|tmpPackage class|
	self addXCategory.
	self addXMatchCategory.
	
	tmpPackage := self organizer packageNamed: #'XXXXX'.
	class := self createNewTraitNamed: 'NewClass' inCategory: 'XXXXX-YYYY'.

	self assert: (tmpPackage definesClass: class).
	self assert: tmpPackage equals: class package.