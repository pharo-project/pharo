"
This class contains tests for `FluidTraitBuilder`
"
Class {
	#name : 'FluidTraitBuilderTest',
	#superclass : 'FluidClassBuilderAbstractTest',
	#category : 'FluidClassBuilder-Tests-Base',
	#package : 'FluidClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
FluidTraitBuilderTest >> setUp [

	super setUp.
	builder := FluidTraitBuilder new
]

{ #category : 'running' }
FluidTraitBuilderTest >> tearDown [

	"self class environment
		at: #TTestTrait
		ifPresent: [ :cl | cl removeFromSystem  ]."
	super tearDown
]

{ #category : 'tests - classBuilder generation' }
FluidTraitBuilderTest >> testBuildSimplePoint2 [

	| trait |
	builder := Trait << #TPoint2
					slots: { #x . #y };
					package: self packageNameForTest.
	trait := builder build.

	self assert: trait name equals: #TPoint2.
	self assert: trait slots size equals: 2
]

{ #category : 'class side' }
FluidTraitBuilderTest >> testClassSlots [

	builder classSlots: {#string . #runs}.
	self assert: builder classSlotsToBuild equals: {#string => InstanceVariableSlot. #runs => InstanceVariableSlot}
]

{ #category : 'tests - class creation' }
FluidTraitBuilderTest >> testCreatingEmptyTraitHasDefaultElements [

	| instBuilder newTrait |
	instBuilder := Trait << #TTestTrait package: self packageNameForTest.

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newTrait := instBuilder install.

	self assert: newTrait name equals: #TTestTrait.
	self assert: newTrait slotNames equals: #(  ).
	self assert: newTrait traitComposition equals: {  } asTraitComposition.
	self assert: newTrait class traitComposition equals: {  } asTraitComposition.
	self assert: newTrait classVarNames equals: #(  ).
	self assert: newTrait category equals: self packageNameForTest
]

{ #category : 'tests - class creation' }
FluidTraitBuilderTest >> testCreatingFullTraitHasAllElements [

	| instBuilder newTrait |
	self categoryHack.
	instBuilder := (Trait << #TTestTrait)
		               slots: #( a b c );
		               traits: { TViewModelMock };
		               tag: 'lala';
		               package: self packageNameForTest.

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newTrait := instBuilder install.

	self assert: newTrait name equals: #TTestTrait.
	self assert: newTrait slotNames equals: #( a b c ).
	self assert: newTrait traitComposition equals: { TViewModelMock } asTraitComposition.
	self assert: newTrait class traitComposition equals: { TViewModelMock classSide } asTraitComposition.
	self assert: newTrait classVarNames equals: #(  ).
	self assert: newTrait category equals: self packageNameForTest , '-lala'
]

{ #category : 'tests - class creation' }
FluidTraitBuilderTest >> testExistingTraitWithClassSlotsArePreservedIfChangingInstanceSide [

	| instBuilder newTrait |
	instBuilder := (Trait << #TTestTrait)
		               classSlots: #( AAA );
		               package: self packageNameForTest.

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newTrait := instBuilder install.

	instBuilder := (Trait << #TTestTrait)
		               slots: #( aaa );
		               package: self packageNameForTest.

	newTrait := instBuilder install.

	self assert: newTrait class slotNames equals: #( AAA )
]

{ #category : 'tests - class creation' }
FluidTraitBuilderTest >> testExistingTraitWithSlotsArePreservedIfChangingClassSide [

	| instBuilder newTrait classBuilder |
	instBuilder := (Trait << #TTestTrait)
		               slots: #( aaa );
		               package: self packageNameForTest.

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newTrait := instBuilder install.

	classBuilder := (Trait << newTrait classTrait)
		                slots: #( AAA );
		                package: self packageNameForTest.

	newTrait := classBuilder install.

	self assert: newTrait class slotNames equals: #( AAA ).
	self assert: newTrait slotNames equals: #( aaa )
]

{ #category : 'tests - classBuilder generation' }
FluidTraitBuilderTest >> testFillShiftClassBuilder [

	| shift |
	self categoryHack.
	builder := (Trait << #TPoint2)
		           slots: { #x. #y };
		           traits: { TViewModelMock };
		           tag: 'Mafia';
		           package: self packageNameForTest;
		           fillShiftClassBuilder;
		           yourself.
	shift := builder shiftClassBuilder.
	self assert: shift name equals: #TPoint2.
	self assert: shift slots size equals: 2.
	self assert: shift category equals: self packageNameForTest , '-Mafia'
]

{ #category : 'tests - install' }
FluidTraitBuilderTest >> testInstallMinimalMockClass [

	| shiftClassBuilder installedClass |
	builder := self class compilerClass new evaluate: 'Trait << #TMyClass
	traits: {};
	slots: {};
	tag: '''' ;
	package: ''' , self packageNameForTest , ''''.
	shiftClassBuilder := builder shiftClassBuilder.
	ShiftClassInstaller new makeWithBuilder: shiftClassBuilder.

	installedClass := self class environment at: #TMyClass ifAbsent: [ self fail ].

	self assert: installedClass name equals: #TMyClass.
	self assert: installedClass isFixed.
	self assert: installedClass slots isEmpty.
	self assert: installedClass traitComposition isEmpty
]

{ #category : 'tests - install' }
FluidTraitBuilderTest >> testInstallSimplePoint2 [

	| trait |
	self class environment at: #TPoint2 ifPresent: [ self fail ].

	builder := (Trait << #TPoint2)
		           slots: { #x. #y };
		           package: self packageNameForTest.
	builder install.
	trait := self class environment at: #TPoint2.
	self assert: trait name equals: #TPoint2.
	self assert: trait slots size equals: 2
]

{ #category : 'tests' }
FluidTraitBuilderTest >> testTraitNoSlotsNoUse [

	| trait |
	builder := Trait << #TPoint3 package: 'Fake'.

	trait := builder build.

	self assert: trait name equals: #TPoint3.
	self assert: trait package name equals: '_UnpackagedPackage'
]

{ #category : 'tests' }
FluidTraitBuilderTest >> testTraitUse [

	| trait |
	builder := (Trait << #TPoint3)
		           traits: TViewModelMock;
		           package: 'Fake'.
	trait := builder build.

	self assert: trait name equals: #TPoint3.
	self assert: trait traitComposition innerClass equals: TViewModelMock.
	self assert: trait package name equals: '_UnpackagedPackage'
]
