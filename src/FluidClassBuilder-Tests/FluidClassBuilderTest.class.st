"
This class contains tests for `FluidClassBuilder`
"
Class {
	#name : #FluidClassBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'FluidClassBuilder-Tests-Base'
}

{ #category : #accessing }
FluidClassBuilderTest >> builder [

	^ builder
]

{ #category : #'private - cleanup' }
FluidClassBuilderTest >> removeTestArtifactsFromSystem [

	self class environment
		at: #MyClass
		ifPresent: [ :p | p removeFromSystem: false ]
		ifAbsent: [  ]
]

{ #category : #running }
FluidClassBuilderTest >> setUp [

	super setUp.
	builder := FluidClassBuilder new
		           superclassToBuild: Object;
		           nameToBuild: #Point33;
		           package: 'FakedPackage'
]

{ #category : #running }
FluidClassBuilderTest >> tearDown [

	self class environment at: #Point2 ifPresent: [ :cl | cl removeFromSystem ].

	(Smalltalk organization packageNamed: #Mock ifAbsent: [ nil ]) ifNotNil: [ :x | x removeFromSystem ].

	(Smalltalk organization packageNamed: #FakedCore ifAbsent: [ nil ]) ifNotNil: [ :x | x removeFromSystem ].



	super tearDown
]

{ #category : #'tests - hidden protocols' }
FluidClassBuilderTest >> testBuildClassSlotsAPI [

	| clas |
	builder := Object << #Point2.
	builder slots: { #x }.
	builder classSlots: {#classX . #classY}.
	"pay attention classSlots: is not to be sent by the user.
	users should only send slots:"
	builder package: 'FakedCore'.
	clas := builder build.

	self assert: clas superclass equals: Object.
	self assert: clas name equals: #Point2.
	self assert: clas slots size equals: 1.
	self assert: clas class slots size equals: 2
]

{ #category : #'tests - hidden protocols' }
FluidClassBuilderTest >> testBuildClassTraitsAPI [

	| clas |
	builder := Object << #Point2.
	builder classTraits: { TViewModelMock2 classTrait }.
	"pay attention classTraits: is not to be sent by the user.
	users should only send traits:"
	builder package: 'FakedCore'.
	clas := builder build.

	self assert: clas superclass equals: Object.
	self assert: clas name equals: #Point2.
	self assert: clas class traitComposition equals: { TViewModelMock2 classTrait } asTraitComposition
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuildLayout [

	builder
		layout: VariableLayout;
		package: 'Collections-Sequenceable'.

	self assert: builder layoutToBuild equals: VariableLayout
]

{ #category : #'tests - classBuilder generation' }
FluidClassBuilderTest >> testBuildSimplePoint2 [

	| clas |
	builder := Object << #Point2.
	builder slots: { #x . #y }.
	builder package: 'FakedCore'.
	clas := builder build.

	self assert: clas superclass equals: Object.
	self assert: clas name equals: #Point2.
	self assert: clas slots size equals: 2.
	self flag: #askPablo.
	"self assert: clas package packageName equals: 'FakedCore'"
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderSharedPools [

	builder slots: {#string . #runs};
		tag: 'Base';
		sharedPools: 'TextConstants';
		package: 'Text-Core'.

	self assert: builder sharedPoolsToBuild equals: 'TextConstants'
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderSharedVariables [

	builder
		layout: ByteLayout;
		sharedVariables: { #NonAsciiMap };
		package: 'Collections-Strings'.

	self assert: builder sharedVariablesToBuild equals: { #NonAsciiMap }.
	self assert: builder layoutToBuild equals: ByteLayout
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderTraits [

	builder traits: TViewModelMock.
	self assert: builder traitsToBuild equals: TViewModelMock asTraitComposition.
	builder fillShiftClassBuilder
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderTraitsClass [

	builder traits: TViewModelMock class.
	self assert: builder traitsToBuild equals: TViewModelMock class asTraitComposition.
	builder fillShiftClassBuilder
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderTraitsWithComposition [

	builder traits: TViewModelMock + TViewModelMock2.
	self
		assert: builder traitsToBuild
		equals: TViewModelMock + TViewModelMock2.
	builder fillShiftClassBuilder
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testBuilderWithPackage [

	builder package: 'Kernel-BasicObjects'.
	self
		assert: builder packageToBuild
		equals: 'Kernel-BasicObjects'
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderWithSlots [

	builder slots: { #x . #y }.
	self assert: builder slotsToBuild equals: { #x => InstanceVariableSlot. #y => InstanceVariableSlot }
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderWithTag [

	builder tag: 'Foo'.

	self assert: builder tagToBuild equals: 'Foo'
]

{ #category : #'tests - class creation' }
FluidClassBuilderTest >> testChevronIsWorkingOnClassSide [

	| instBuilder newClass clasBuilder |
	Smalltalk organization registerPackageNamed: #Mock.

	instBuilder := (Object << #Point2)
		               slots: { #a. #b };
		               layout: WeakLayout;
		               traits: { TViewModelMock };
		               sharedVariables: { #AAA };
		               sharedPools: { #TextConstants };
		               tag: 'boring';
		               package: 'Mock'.

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newClass := instBuilder install.

	clasBuilder := Object class << newClass class traits: { TViewModelMock classTrait }.

	self assert: clasBuilder packageToBuild equals: instBuilder packageToBuild.
	self assert: clasBuilder superclassToBuild equals: instBuilder superclassToBuild.
	self assert: clasBuilder layoutToBuild equals: instBuilder layoutToBuild.
	self assert: clasBuilder sharedVariablesToBuild equals: instBuilder sharedVariablesToBuild.
	self assert: clasBuilder sharedPoolsToBuild equals: instBuilder sharedPoolsToBuild.

	self assert: clasBuilder slotsToBuild equals: instBuilder slotsToBuild.
	self assert: clasBuilder tagToBuild equals: instBuilder tagToBuild.
	self assert: clasBuilder traitsToBuild name equals: TViewModelMock name.
	self assert: clasBuilder nameToBuild equals: instBuilder nameToBuild.

	self assert: clasBuilder classSlotsToBuild equals: instBuilder classSlotsToBuild.
	self assert: clasBuilder classTraitsToBuild equals: instBuilder classTraitsToBuild
]

{ #category : #'tests - class creation' }
FluidClassBuilderTest >> testChevronIsWorkingOnClassSideOnEmpty [

	| instBuilder newClass clasBuilder |
	instBuilder := (Object << #Point2
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newClass := instBuilder install.

	clasBuilder := Object class << newClass class.

	self assert: clasBuilder packageToBuild equals: instBuilder packageToBuild.
	self assert: clasBuilder superclassToBuild equals: instBuilder superclassToBuild.
	self assert: clasBuilder layoutToBuild equals: instBuilder layoutToBuild.
	self assert: clasBuilder sharedVariablesToBuild equals: instBuilder sharedVariablesToBuild.
	self assert: clasBuilder sharedPoolsToBuild equals: instBuilder sharedPoolsToBuild.

	self assert: clasBuilder slotsToBuild equals: instBuilder slotsToBuild.
	self assert: clasBuilder tagToBuild equals: instBuilder tagToBuild.
	self assert: clasBuilder traitsToBuild equals: instBuilder traitsToBuild.
	self assert: clasBuilder nameToBuild equals: instBuilder nameToBuild.

	self assert: clasBuilder classSlotsToBuild equals: instBuilder classSlotsToBuild.
	self assert: clasBuilder classTraitsToBuild equals: instBuilder classTraitsToBuild
]

{ #category : #'tests - class creation' }
FluidClassBuilderTest >> testClassSlots [

	builder classSlots: {#string . #runs}.
	self assert: builder classSlotsToBuild equals: {#string => InstanceVariableSlot. #runs => InstanceVariableSlot}
]

{ #category : #'tests - class creation' }
FluidClassBuilderTest >> testClassTraits [

	builder classTraits: {TViewModel classSide}.
	self assert: builder classTraitsToBuild equals: {TViewModel classSide} asTraitComposition
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testCreateBuilder [

	builder := Object << #Point.

	self assert: builder superclassToBuild equals: Object.
	self assert: builder nameToBuild equals: #Point
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testCreateBuilderWithNil [

	builder := nil << #NewProtoObject.

	self assert: builder superclassToBuild equals: nil.
	self assert: builder nameToBuild equals: #NewProtoObject
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testCreateClassWithFullExpandedDefinitionKeepsTheMinimum [

	"check ClassDescription>>#definitionFullExpanded"

	| shiftClassBuilder |
	builder := self class compilerClass new
		           evaluate: 'Object << #MyClass
	layout: FixedLayout;
	traits: {};
	slots: {};
	sharedVariables: {};
	tag: '''' ;
	sharedPools: {};
	package: ''Mock'''.
	builder build.
	shiftClassBuilder := builder shiftClassBuilder.

	self assert: shiftClassBuilder superclass equals: Object.
	self assert: shiftClassBuilder name equals: #MyClass.
	self assert: shiftClassBuilder layoutDefinition slots isEmpty.
	"we should add slots to ShiftClassBuilder"
	self flag: #Pablo.
	self assert: shiftClassBuilder traitComposition isEmpty.
	self assert: shiftClassBuilder layoutDefinition sharedVariables isEmpty.
	self assert: shiftClassBuilder layoutDefinition sharedPools isEmpty
]

{ #category : #'tests - class creation' }
FluidClassBuilderTest >> testCreatedClassHasAllElements [

	| instBuilder newClass |
	instBuilder := (Object << #Point2
		slots: { #a. #b };
		layout: WeakLayout;
		traits: {TViewModelMock };
		sharedVariables: { #AAA};
		sharedPools: { #TextConstants};
		tag: 'boring' ;
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newClass := instBuilder install.

	self assert: newClass name equals: #Point2.
	self assert: newClass slots size equals: 2.
	self assert: newClass slotNames equals: #(a b).
	self assert: newClass classLayout class equals: WeakLayout.
	self assert: newClass traitComposition equals: {TViewModelMock} asTraitComposition.
	self assert: newClass class traitComposition equals: {TViewModelMock classSide} asTraitComposition.
	self assert: newClass classVarNames equals: #(AAA).
	self assertCollection: newClass sharedPools hasSameElements: {TextConstants}.
	self assert: newClass category equals: 'Mock-boring'
]

{ #category : #'tests - class creation' }
FluidClassBuilderTest >> testCreatedEmptyClassHasDefaultElements [

	| instBuilder newClass |
	instBuilder := (Object << #Point2
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newClass := instBuilder install.

	self assert: newClass name equals: #Point2.
	self assert: newClass slotNames equals: #().
	self assert: newClass classLayout class equals: FixedLayout.
	self assert: newClass traitComposition equals: {} asTraitComposition.
	self assert: newClass class traitComposition equals: {} asTraitComposition.
	self assert: newClass classVarNames equals: #().
	self assertCollection: newClass sharedPools hasSameElements: {}.
	self assert: newClass category equals: 'Mock'
]

{ #category : #'tests - class creation' }
FluidClassBuilderTest >> testExistingClassWithClassSlot [

	| instBuilder newClass |

	"We are creating a class with a class slot. This class slot is defined in the class side of the fluid definition.
	When redefining the instance side, the class slot should not be lost."

	instBuilder := (Object << #Point2
		slots: { #a. #b };
		layout: WeakLayout;
		traits: {TViewModelMock };
		sharedVariables: { #BBB};
		classSlots: #(AAA);
		sharedPools: { #TextConstants};
		tag: 'boring' ;
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newClass := instBuilder install.

	self assert: newClass class slots first name equals: #AAA.

	instBuilder := (Object << #Point2
		slots: { #a. #b. #c };
		layout: WeakLayout;
		traits: {TViewModelMock };
		sharedVariables: { #BBB};
		sharedPools: { #TextConstants};
		tag: 'boring' ;
		package: 'Mock').

	newClass := instBuilder install.

	self assert: newClass class slots first name equals: #AAA
]

{ #category : #'tests - class creation' }
FluidClassBuilderTest >> testExistingClassWithClassSlotThenWeRemoveIt [

	| instBuilder newClass classBuilder |

	"We are creating a class with a class slot. This class slot is defined in the class side of the fluid definition.
	Then we are removing the classSlot, it should remove it"

	instBuilder := (Object << #Point2
		slots: { #a. #b };
		layout: WeakLayout;
		traits: {TViewModelMock };
		sharedVariables: { #BBB};
		classSlots: #(AAA);
		sharedPools: { #TextConstants};
		tag: 'boring' ;
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newClass := instBuilder install.

	self assert: newClass class slots first name equals: #AAA.

	classBuilder := Object class << newClass class.

	newClass := classBuilder install.

	self assert: newClass class slots equals: #()
]

{ #category : #'tests - classBuilder generation' }
FluidClassBuilderTest >> testFillShiftClassBuilder [

	| shift |
	builder := Object << #Point2.
	builder slots: { #x . #y }.
	builder sharedVariables: {  #Share1 . #Share2 }.
	builder sharedPools: { ChronologyConstants }.
	builder tag: 'Mafia'.
	builder package: 'FakedCore'.
	builder fillShiftClassBuilder.
	shift := builder shiftClassBuilder.
	self assert: shift superclass equals: Object.
	self assert: shift name equals: #Point2.
	self assert: shift slots size equals: 2.
	self assert: shift sharedPools equals: {ChronologyConstants}.
	self assert: shift category equals: 'FakedCore-Mafia'.
	self flag: #askPablo.
	"self assert: clas package packageName equals: 'FakedCore'"
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testInstallMinimalMockClass [

	| shiftClassBuilder installedClass |
	self removeTestArtifactsFromSystem.
	[
	builder := self class compilerClass new
		           evaluate: 'Object << #MyClass
	layout: FixedLayout;
	traits: {};
	slots: {};
	sharedVariables: {};
	tag: '''' ;
	sharedPools: {};
	package: ''MyPackage'''.
	builder build.
	shiftClassBuilder := builder shiftClassBuilder.
	ShiftClassInstaller new makeWithBuilder: shiftClassBuilder.

	installedClass := self class environment at: #MyClass ifAbsent: [self fail].

	self assert: installedClass superclass equals: Object.
	self assert: installedClass name equals: #MyClass.
	self assert: installedClass isFixed.
	self assert: installedClass slots isEmpty.
	self assert: installedClass traitComposition isEmpty.
	self assert: installedClass classVariables isEmpty.
	self assert: installedClass sharedPools isEmpty]
		ensure: [ self removeTestArtifactsFromSystem ]
]

{ #category : #'tests - classBuilder generation' }
FluidClassBuilderTest >> testInstallSimplePoint2 [

	[
	| pt2Class |
	self assert: (self class environment at: #Point2 ifAbsent: [ true ]).
	builder := Object << #Point2.
	builder slots: { #x. #y }.
	builder package: 'FakedCore'.
	builder install.

	pt2Class := self class environment at: #Point2.
	self assert: pt2Class superclass equals: Object.
	self assert: pt2Class name equals: #Point2.
	self assert: pt2Class slots size equals: 2 ] ensure: [
		(self class environment at: #Point2 ifAbsent: [ self fail ]) removeFromSystem.
		self assert: (self class environment at: #Point2 ifAbsent: [ true ]) ]
]
