"
This class contains tests for `FluidClassBuilder`
"
Class {
	#name : 'ShiftClassBuilderTest',
	#superclass : 'ShiftClassBuilderAbstractTest',
	#category : 'Shift-ClassBuilder-Tests-Base',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
ShiftClassBuilderTest >> setUp [

	super setUp.
	builder := ShiftClassBuilder new
		           superclass: Object;
		           name: #Point33;
		           package: 'FakedPackage'
]

{ #category : 'tests - hidden protocols' }
ShiftClassBuilderTest >> testBuildClassSlotsAPI [

	| clas |
	builder := Object << #Point2.
	builder slots: { #x }.
	builder classSlots: {#classX . #classY}.
	"pay attention classSlots: is not to be sent by the user.
	users should only send slots:"
	builder package: self packageNameForTest.
	clas := builder build.

	self assert: clas superclass equals: Object.
	self assert: clas name equals: #Point2.
	self assert: clas slots size equals: 1.
	self assert: clas class slots size equals: 2
]

{ #category : 'tests - hidden protocols' }
ShiftClassBuilderTest >> testBuildClassTraitsAPI [

	| clas |
	builder := Object << #Point2.
	builder classTraits: { TViewModelMock2 classTrait }.
	"pay attention classTraits: is not to be sent by the user.
	users should only send traits:"
	builder package: self packageNameForTest.
	clas := builder build.

	self assert: clas superclass equals: Object.
	self assert: clas name equals: #Point2.
	self assert: clas class traitComposition equals: { TViewModelMock2 classTrait } asTraitComposition
]

{ #category : 'tests - gathering' }
ShiftClassBuilderTest >> testBuildLayout [

	builder
		layout: VariableLayout;
		package: 'Collections-Sequenceable'.

	self assert: builder layoutClass equals: VariableLayout
]

{ #category : 'tests - classBuilder generation' }
ShiftClassBuilderTest >> testBuildSimplePoint2 [

	| clas |
	builder := Object << #Point2.
	builder slots: { #x. #y }.
	builder package: self packageNameForTest.
	clas := builder build.

	self assert: clas superclass equals: Object.
	self assert: clas name equals: #Point2.
	self assert: clas slots size equals: 2.

	"for now, the class will only know the package if it is installed"
	self assert: clas package name equals: UndefinedPackage undefinedPackageName
]

{ #category : 'tests - gathering' }
ShiftClassBuilderTest >> testBuilderSharedPools [

	builder
		slots: { #string. #runs };
		tag: 'Base';
		sharedPools: 'TextConstants';
		package: 'Text-Core'.

	self assert: builder sharedPools equals: #( TextConstants )
]

{ #category : 'tests - gathering' }
ShiftClassBuilderTest >> testBuilderSharedVariables [

	builder
		layout: ByteLayout;
		sharedVariables: { #NonAsciiMap };
		package: 'Collections-Strings'.

	self assert: (builder sharedVariables collect: [ :var | var name ]) equals: { #NonAsciiMap }.
	self assert: builder layoutClass equals: ByteLayout
]

{ #category : 'tests - gathering' }
ShiftClassBuilderTest >> testBuilderTraits [

	builder traits: TViewModelMock.
	self assert: builder traitComposition equals: TViewModelMock asTraitComposition
]

{ #category : 'tests - gathering' }
ShiftClassBuilderTest >> testBuilderTraitsWithComposition [

	builder traits: TViewModelMock + TViewModelMock2.
	self assert: builder traitComposition equals: TViewModelMock + TViewModelMock2
]

{ #category : 'tests - mandatory' }
ShiftClassBuilderTest >> testBuilderWithPackage [

	builder package: 'Kernel-BasicObjects'.
	self
		assert: builder package
		equals: 'Kernel-BasicObjects'
]

{ #category : 'tests - gathering' }
ShiftClassBuilderTest >> testBuilderWithSlots [

	builder slots: { #x . #y }.
	self assert: builder slots equals: { #x => InstanceVariableSlot. #y => InstanceVariableSlot }
]

{ #category : 'tests - gathering' }
ShiftClassBuilderTest >> testBuilderWithTag [

	builder tag: 'Foo'.

	self assert: builder tag equals: 'Foo'
]

{ #category : 'tests - class creation' }
ShiftClassBuilderTest >> testChevronIsWorkingOnClassSide [

	| instBuilder newClass clasBuilder |

	instBuilder := (Object << #Point2)
		               slots: { #a. #b };
		               layout: WeakLayout;
		               traits: { TViewModelMock };
		               sharedVariables: { #AAA };
		               sharedPools: { #TextConstants };
		               tag: 'boring';
		               package: self packageNameForTest.

	newClass := instBuilder install.

	clasBuilder := Object class << newClass class traits: { TViewModelMock classTrait }.

	self assert: clasBuilder package equals: instBuilder package.
	self assert: clasBuilder superclass equals: instBuilder superclass.
	self assert: clasBuilder layoutClass equals: instBuilder layoutClass.
	self assert: clasBuilder sharedVariables equals: instBuilder sharedVariables.
	self assert: clasBuilder sharedPools equals: instBuilder sharedPools.

	self assert: clasBuilder slots equals: instBuilder slots.
	self assert: clasBuilder tag equals: instBuilder tag.
	self assert: clasBuilder traitComposition name equals: TViewModelMock name.
	self assert: clasBuilder name equals: instBuilder name.

	self assert: clasBuilder classSlots equals: instBuilder classSlots.
	self assert: clasBuilder classTraitComposition equals: instBuilder classTraitComposition
]

{ #category : 'tests - class creation' }
ShiftClassBuilderTest >> testChevronIsWorkingOnClassSideOnEmpty [

	| instBuilder newClass clasBuilder |
	instBuilder := Object << #Point2 package: self packageNameForTest.

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newClass := instBuilder install.

	clasBuilder := Object class << newClass class.

	self assert: clasBuilder package equals: instBuilder package.
	self assert: clasBuilder superclass equals: instBuilder superclass.
	self assert: clasBuilder layoutClass equals: instBuilder layoutClass.
	self assert: clasBuilder sharedVariables equals: instBuilder sharedVariables.
	self assert: clasBuilder sharedPools equals: instBuilder sharedPools.

	self assert: clasBuilder slots equals: instBuilder slots.
	self assert: clasBuilder tag equals: instBuilder tag.
	self assert: clasBuilder traitComposition equals: instBuilder traitComposition.
	self assert: clasBuilder name equals: instBuilder name.

	self assert: clasBuilder classSlots equals: instBuilder classSlots.
	self assert: clasBuilder classTraitComposition equals: instBuilder classTraitComposition
]

{ #category : 'tests - class creation' }
ShiftClassBuilderTest >> testClassSlots [

	builder classSlots: {#string . #runs}.
	self assert: builder classSlots equals: {#string => InstanceVariableSlot. #runs => InstanceVariableSlot}
]

{ #category : 'tests - class creation' }
ShiftClassBuilderTest >> testClassTraits [

	builder classTraits: {TViewModel classSide}.
	self assert: builder classTraitComposition equals: {TViewModel classSide} asTraitComposition
]

{ #category : 'tests - mandatory' }
ShiftClassBuilderTest >> testCreateBuilder [

	builder := Object << #ShiftClassBuilder.

	self assert: builder superclass equals: Object.
	self assert: builder name equals: #ShiftClassBuilder
]

{ #category : 'tests - mandatory' }
ShiftClassBuilderTest >> testCreateBuilderWithNil [

	builder := nil << #NewProtoObject.

	self assert: builder superclass isNil.
	self assert: builder name equals: #NewProtoObject
]

{ #category : 'tests - mandatory' }
ShiftClassBuilderTest >> testCreateClassWithFullExpandedDefinitionKeepsTheMinimum [
	"check ClassDescription>>#definitionFullExpanded"

	builder := self class compilerClass new evaluate: 'Object << #MyClass
	layout: FixedLayout;
	traits: {};
	slots: {};
	sharedVariables: {};
	tag: '''' ;
	sharedPools: {};
	package: ''' , self packageNameForTest , ''''.
	builder build.

	self assert: builder superclass equals: Object.
	self assert: builder name equals: #MyClass.
	self assert: builder layoutDefinition slots isEmpty.
	self assert: builder traitComposition isEmpty.
	self assert: builder layoutDefinition sharedVariables isEmpty.
	self assert: builder layoutDefinition sharedPools isEmpty
]

{ #category : 'tests - class creation' }
ShiftClassBuilderTest >> testCreatedEmptyClassHasDefaultElements [

	| instBuilder newClass |
	instBuilder := Object << #Point2 package: self packageNameForTest.

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newClass := instBuilder install.

	self assert: newClass name equals: #Point2.
	self assert: newClass slotNames equals: #(  ).
	self assert: newClass classLayout class equals: FixedLayout.
	self assert: newClass traitComposition equals: {  } asTraitComposition.
	self assert: newClass class traitComposition equals: {  } asTraitComposition.
	self assert: newClass classVarNames equals: #(  ).
	self assertCollection: newClass sharedPools hasSameElements: {  }.
	self assert: newClass package name equals: self packageNameForTest
]

{ #category : 'tests - class creation' }
ShiftClassBuilderTest >> testExistingClassWithClassSlot [

	| instBuilder newClass |
	"We are creating a class with a class slot. This class slot is defined in the class side of the fluid definition.
	When redefining the instance side, the class slot should not be lost."
	instBuilder := (Object << #Point2)
		               slots: { #a. #b };
		               layout: WeakLayout;
		               traits: { TViewModelMock };
		               sharedVariables: { #BBB };
		               classSlots: #( AAA );
		               sharedPools: { #TextConstants };
		               tag: 'boring';
		               package: self packageNameForTest.

	newClass := instBuilder install.

	self assert: newClass class slots first name equals: #AAA.

	instBuilder := (Object << #Point2)
		               slots: { #a. #b. #c };
		               layout: WeakLayout;
		               traits: { TViewModelMock };
		               sharedVariables: { #BBB };
		               sharedPools: { #TextConstants };
		               tag: 'boring';
		               package: self packageNameForTest.

	newClass := instBuilder install.

	self assert: newClass class slots first name equals: #AAA
]

{ #category : 'tests - class creation' }
ShiftClassBuilderTest >> testExistingClassWithClassSlotThenWeRemoveIt [

	| instBuilder newClass classBuilder |
	"We are creating a class with a class slot. This class slot is defined in the class side of the fluid definition.
	Then we are removing the classSlot, it should remove it"
	instBuilder := (Object << #Point2)
		               slots: { #a. #b };
		               layout: WeakLayout;
		               traits: { TViewModelMock };
		               sharedVariables: { #BBB };
		               classSlots: #( AAA );
		               sharedPools: { #TextConstants };
		               tag: 'boring';
		               package: self packageNameForTest.

	newClass := instBuilder install.

	self assert: newClass class slots first name equals: #AAA.

	classBuilder := Object class << newClass class.

	newClass := classBuilder install.

	self assert: newClass class slots equals: #(  )
]

{ #category : 'tests - classBuilder generation' }
ShiftClassBuilderTest >> testFillShiftClassBuilder [

	builder := (Object << #Point2)
		           slots: { #x. #y };
		           sharedVariables: { #Share1. #Share2 };
		           sharedPools: { ChronologyConstants };
		           tag: 'Mafia';
		           package: self packageNameForTest.
	self assert: builder superclass equals: Object.
	self assert: builder name equals: #Point2.
	self assert: builder slots size equals: 2.
	self assert: builder sharedPools equals: { #ChronologyConstants }.
	self assert: builder package equals: self packageNameForTest.
	self assert: builder tag equals: #Mafia
]

{ #category : 'tests - mandatory' }
ShiftClassBuilderTest >> testInstallMinimalMockClass [

	| installedClass |
	builder := self class compilerClass new evaluate: 'Object << #MyClass
	layout: FixedLayout;
	traits: {};
	slots: {};
	sharedVariables: {};
	tag: '''' ;
	sharedPools: {};
	package: ''' , self packageNameForTest , ''''.
	ShiftClassInstaller new makeWithBuilder: builder.

	installedClass := self class environment at: #MyClass ifAbsent: [ self fail ].

	self assert: installedClass superclass equals: Object.
	self assert: installedClass name equals: #MyClass.
	self assert: installedClass isFixed.
	self assert: installedClass slots isEmpty.
	self assert: installedClass traitComposition isEmpty.
	self assert: installedClass classVariables isEmpty.
	self assert: installedClass sharedPools isEmpty
]

{ #category : 'tests - classBuilder generation' }
ShiftClassBuilderTest >> testInstallSimplePoint2 [

	[
	| pt2Class |
	self assert: (self class environment at: #Point2 ifAbsent: [ true ]).
	builder := Object << #Point2.
	builder slots: { #x. #y }.
	builder package: self packageNameForTest.
	builder install.

	pt2Class := self class environment at: #Point2.
	self assert: pt2Class superclass equals: Object.
	self assert: pt2Class name equals: #Point2.
	self assert: pt2Class slots size equals: 2 ] ensure: [
		(self class environment at: #Point2 ifAbsent: [ self fail ]) removeFromSystem.
		self assert: (self class environment at: #Point2 ifAbsent: [ true ]) ]
]
