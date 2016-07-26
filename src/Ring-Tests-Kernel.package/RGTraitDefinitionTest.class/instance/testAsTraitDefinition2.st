testing
testAsTraitDefinition2

	| newTrait newClass newSlice |
	newClass := Trait asRingDefinitionWithMethods: false withSuperclasses: false withSubclasses: true withPackages: false.
	newSlice := newClass environment.
	self assert: (newClass allSubclasses size = Smalltalk globals environment allTraits size).
	self assert: newClass traitNames size = 1.
	self assert: newClass traits first = (newSlice traitNamed: #TClass).
	
	newTrait := TBehaviorCategorization asRingDefinitionWithMethods: true withSuperclasses: true withSubclasses: false withPackages: true.
	newSlice := newTrait environment.
	self assert: newTrait superclass = (newSlice classNamed: #Trait).
	self assert: newTrait methods size < newSlice methods size.
	self assert: newTrait category = #'Traits-Kernel-Traits'.
	self assert: newTrait package = (newSlice packageNamed: #Traits).
	self assert: newTrait subclasses isEmpty.