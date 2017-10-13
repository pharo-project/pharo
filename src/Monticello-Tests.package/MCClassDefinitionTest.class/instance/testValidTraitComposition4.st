tests
testValidTraitComposition4
	"Related to http://code.google.com/p/pharo/issues/detail?id=2598"

	| d className cls |
	className := 'MCMockClassC'.
	d := MCClassDefinition
		name: className
		superclassName: 'Object'
		traitComposition: 'Trait1 - {#c1}'
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
	self assert: (cls selectors includesAll: {#c}).
	self deny: (cls selectors includesAnyOf: {#c1})