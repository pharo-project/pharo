Extension { #name : #RGClassDefinitionTest }

{ #category : #'*Ring-Definitions-Tests-Monticello' }
RGClassDefinitionTest >> testAsClassDefinition3 [

	| newClass newSlice |
	newClass:= OrderedCollection asRingDefinitionWithMethods: true withSuperclasses: false withSubclasses: false withPackages: false.
	newSlice := newClass environment.
	self assert: (newClass methods size = OrderedCollection methods size).
	self assert: newSlice isSlice.
	self assert: newSlice classes size = 1.
	self assert: (newSlice classNamed: #OrderedCollection) = newClass.
	
	newClass:= OrderedCollection asRingDefinitionWithMethods: false withSuperclasses: true withSubclasses: false withPackages: false.
	newSlice := newClass environment.
	self assert: (newClass superclass isRingObject).
	self assert: (newClass superclass name = OrderedCollection superclass name).
	self assert: (newClass allSuperclasses size = OrderedCollection allSuperclasses size).
	self assert: newClass superclass = (newSlice classNamed: newClass superclassName).
	
	newClass:= RGBehaviorDefinition asRingDefinitionWithMethods: false withSuperclasses: false withSubclasses: true withPackages: false.
	newSlice := newClass environment.
	self assert: (newClass subclasses size = RGBehaviorDefinition subclasses size).
	self assert: (newClass allSubclasses size = RGBehaviorDefinition allSubclasses size).
	self assert: (newClass subclasses allSatisfy:[ :each| newSlice classes values includes: each ]).

	newClass:= RGBehaviorDefinition asRingDefinitionWithMethods: false withSuperclasses: false withSubclasses: false withPackages: true.
	newSlice := newClass environment.
	self assert: (newClass package name = #'Ring-Definitions-Core').
	self assert: (newSlice packageNamed: #'Ring-Definitions-Core') = newClass package.
	self assert: (newClass package classes size = 1).
	self assert: (newClass package classes values first = newClass).

	newClass:= Class asRingDefinitionWithMethods: true withSuperclasses: false withSubclasses: false withPackages: true.
	newSlice := newClass environment.
	self assert: (newClass package name = #Kernel).
	self assert: (newClass extensionMethods notEmpty).
	self assert: (newSlice methods size = (newClass methods size + newClass classSide methods size)).
	self assert: (newSlice packageNamed: #'Ring-Definitions-Core') methods size > 1.

]

{ #category : #'*Ring-Definitions-Tests-Monticello' }
RGClassDefinitionTest >> testAsFullClassDefinition [
	| rgClass |
	rgClass := Class asRingDefinition.
	self assertEmpty: rgClass methods.
	self assert: rgClass superclass isNil.
	self assertEmpty: rgClass subclasses.
	self assert: rgClass package name equals: #Kernel.

	rgClass := Class asFullRingDefinition.
	self denyEmpty: rgClass methods.
	self assert: (rgClass methodNamed: #asRingDefinition) package name = #'Ring-Definitions-Core'.
	self assert: rgClass superclass notNil.
	self assert: rgClass superclass name = #ClassDescription.
	self denyEmpty: rgClass subclasses.
	self assert: rgClass package notNil.
	self assert: rgClass package = rgClass instanceSide package.
	self assert: rgClass package name = #Kernel.
	self assert: rgClass category = #'Kernel-Classes'.
	self denyEmpty: rgClass extensionMethods.

	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #Kernel.
	self assert: rgClass subclasses first package name equals: #Kernel.

	rgClass := RGClassDefinition classSide asFullRingDefinition.
	self assert: rgClass package = rgClass instanceSide package
]
