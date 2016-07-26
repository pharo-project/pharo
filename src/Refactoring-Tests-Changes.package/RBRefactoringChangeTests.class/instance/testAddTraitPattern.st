tests-pattern
testAddTraitPattern
	"Make sure that all trait definitions can be parsed."
	
	self exampleTraits do: [ :trait |
		| change |
		change := changes defineClass: trait definition.
		self assert: (change isKindOf: RBAddTraitChange).
		self assert: (change changeClassName = trait name).
		self assert: (change category = trait category).
		self universalTestFor: change ]