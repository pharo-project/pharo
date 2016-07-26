tests
testValidTraitComposition3
	"Related to http://code.google.com/p/pharo/issues/detail?id=2148"

	| d className cls |
	className := 'MCMockClassC'.
	d := MCClassDefinition
		name: className
		superclassName: 'Object'
		traitComposition: 'Trait1 + Trait2'
		classTraitComposition: '{}'
		category: self mockCategoryName
		instVarNames: #()
		classVarNames: #()
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: (self commentForClass: className)
		commentStamp: (self commentStampForClass: className).
	d load.
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	cls := Smalltalk at: #MCMockClassC.
	self assert: (cls includesSelector: #c1).
	self assert: (cls includesSelector: #c2).
	self assert: ((Smalltalk at: #MCMockClassC) traitComposition allTraits includes: Trait1).
	self assert: ((Smalltalk at: #MCMockClassC) traitComposition allTraits includes: Trait2)