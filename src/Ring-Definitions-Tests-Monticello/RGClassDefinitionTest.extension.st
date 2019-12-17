Extension { #name : #RGClassDefinitionTest }

{ #category : #'*Ring-Definitions-Tests-Monticello' }
RGClassDefinitionTest >> testAsClassDefinition3 [
	| newClass newSlice |
	newClass := OrderedCollection
		asRingDefinitionWithMethods: true
		withSuperclasses: false
		withSubclasses: false
		withPackages: false.
	newSlice := newClass environment.
	self assert: newClass methods size equals: OrderedCollection methods size.
	self assert: newSlice isSlice.
	self assert: newSlice classes size equals: 1.
	self assert: (newSlice classNamed: #OrderedCollection) equals: newClass.

	newClass := OrderedCollection
		asRingDefinitionWithMethods: false
		withSuperclasses: true
		withSubclasses: false
		withPackages: false.
	newSlice := newClass environment.
	self assert: newClass superclass isRingObject.
	self assert: newClass superclass name equals: OrderedCollection superclass name.
	self assert: newClass allSuperclasses size equals: OrderedCollection allSuperclasses size.
	self assert: newClass superclass equals: (newSlice classNamed: newClass superclassName).

	newClass := RGBehaviorDefinition
		asRingDefinitionWithMethods: false
		withSuperclasses: false
		withSubclasses: true
		withPackages: false.
	newSlice := newClass environment.
	self assert: newClass subclasses size equals: RGBehaviorDefinition subclasses size.
	self assert: newClass allSubclasses size equals: RGBehaviorDefinition allSubclasses size.
	self assert: (newClass subclasses allSatisfy: [ :each | newSlice classes values includes: each ]).

	newClass := RGBehaviorDefinition
		asRingDefinitionWithMethods: false
		withSuperclasses: false
		withSubclasses: false
		withPackages: true.
	newSlice := newClass environment.
	self assert: newClass package name equals: #'Ring-Definitions-Core'.
	self assert: (newSlice packageNamed: #'Ring-Definitions-Core') equals: newClass package.
	self assert: newClass package classes size equals: 1.
	self assert: newClass package classes values first equals: newClass.

	newClass := Class
		asRingDefinitionWithMethods: true
		withSuperclasses: false
		withSubclasses: false
		withPackages: true.
	newSlice := newClass environment.
	self assert: newClass package name equals: #Kernel.
	self assert: newClass extensionMethods notEmpty.
	self assert: newSlice methods size equals: newClass methods size + newClass classSide methods size.
	self assert: (newSlice packageNamed: #'Ring-Definitions-Core') methods size > 1
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
	self assert: (rgClass methodNamed: #asRingDefinition) package name equals: #'Ring-Definitions-Core'.
	self assert: rgClass superclass notNil.
	self assert: rgClass superclass name equals: #ClassDescription.
	self denyEmpty: rgClass subclasses.
	self assert: rgClass package notNil.
	self assert: rgClass package equals: rgClass instanceSide package.
	self assert: rgClass package name equals: #Kernel.
	self assert: rgClass category equals: #'Kernel-Classes'.
	self denyEmpty: rgClass extensionMethods.

	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #Kernel.
	self assert: rgClass subclasses first package name equals: #Kernel.

	rgClass := RGClassDefinition classSide asFullRingDefinition.
	self assert: rgClass package equals: rgClass instanceSide package
]
