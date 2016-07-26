tests-pattern
testAddClassTraitPattern
	"Make sure that all class trait definitions can be parsed."
	
	self exampleTraits do: [ :trait |
		| change |
		change := changes defineClass: trait classTrait definition.
		self assert: (change isKindOf: RBAddClassTraitChange).
		self assert: (change changeClassName = trait name).
		self universalTestFor: change ]