testing
testAsTraitDefinition2

	| newTrait newClass newSlice |
	newClass := MOPTestClassC asRingDefinitionWithMethods: false withSuperclasses: false withSubclasses: true withPackages: false.
	newSlice := newClass environment.
	self assert: (newClass allSubclasses size = 0).
	self assert: newClass traitNames size = 1.
	self assert: newClass traits first = (newSlice traitNamed: #Trait2).
	
	newTrait := MOPTestClassA asRingDefinitionWithMethods: true withSuperclasses: true withSubclasses: false withPackages: true.
	newSlice := newTrait environment.
	self assert: newTrait superclass = (newSlice classNamed: #Object).
	self assert: newTrait methods size < newSlice methods size.
	self assert: newTrait category = #'Tests-Traits-MOP'.
	self assert: newTrait subclasses isEmpty.