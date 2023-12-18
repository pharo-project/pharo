"
This class contains tests for `FluidTraitBuilder`
"
Class {
	#name : 'ShiftTraitBuilderTest',
	#superclass : 'ShiftClassBuilderAbstractTest',
	#category : 'Shift-ClassBuilder-Tests-Base',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
ShiftTraitBuilderTest >> setUp [

	super setUp.
	builder := ShiftClassBuilder new
		           beTrait;
		           yourself
]

{ #category : 'running' }
ShiftTraitBuilderTest >> tearDown [

	"self class environment
		at: #TTestTrait
		ifPresent: [ :cl | cl removeFromSystem  ]."
	super tearDown
]

{ #category : 'tests - classBuilder generation' }
ShiftTraitBuilderTest >> testBuildSimplePoint2 [

	| trait |
	builder := Trait << #TPoint2
					slots: { #x . #y };
					package: self packageNameForTest.
	trait := builder build.

	self assert: trait name equals: #TPoint2.
	self assert: trait slots size equals: 2
]

{ #category : 'class side' }
ShiftTraitBuilderTest >> testClassSlots [

	builder classSlots: {#string . #runs}.
	self assert: builder classSlots equals: {#string => InstanceVariableSlot. #runs => InstanceVariableSlot}
]

{ #category : 'tests - class creation' }
ShiftTraitBuilderTest >> testCreatingEmptyTraitHasDefaultElements [

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
	self assert: newTrait package name equals: self packageNameForTest
]

{ #category : 'tests - class creation' }
ShiftTraitBuilderTest >> testExistingTraitWithClassSlotsArePreservedIfChangingInstanceSide [

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
ShiftTraitBuilderTest >> testExistingTraitWithSlotsArePreservedIfChangingClassSide [

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
ShiftTraitBuilderTest >> testFillShiftClassBuilder [

	builder := (Trait << #TPoint2)
		           slots: { #x. #y };
		           traits: { TViewModelMock };
		           tag: 'Mafia';
		           package: self packageNameForTest;
		           yourself.
	self assert: builder name equals: #TPoint2.
	self assert: builder slots size equals: 2.
	self assert: builder package equals: self packageNameForTest.
	self assert: builder tag equals: 'Mafia'
]

{ #category : 'tests - install' }
ShiftTraitBuilderTest >> testInstallMinimalMockClass [

	| installedClass |
	builder := self class compilerClass new evaluate: 'Trait << #TMyClass
	traits: {};
	slots: {};
	tag: '''' ;
	package: ''' , self packageNameForTest , ''''.
	
	builder install.
	
	installedClass := self class environment at: #TMyClass ifAbsent: [ self fail ].

	self assert: installedClass name equals: #TMyClass.
	self assert: installedClass isFixed.
	self assert: installedClass slots isEmpty.
	self assert: installedClass traitComposition isEmpty
]

{ #category : 'tests - install' }
ShiftTraitBuilderTest >> testInstallSimplePoint2 [

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
ShiftTraitBuilderTest >> testTraitNoSlotsNoUse [

	| trait |
	builder := Trait << #TPoint3 package: 'Fake'.

	trait := builder build.

	self assert: trait name equals: #TPoint3.
	self assert: trait package name equals: '_UnpackagedPackage'
]

{ #category : 'tests' }
ShiftTraitBuilderTest >> testTraitUse [

	| trait |
	builder := (Trait << #TPoint3)
		           traits: TViewModelMock;
		           package: 'Fake'.
	trait := builder build.

	self assert: trait name equals: #TPoint3.
	self assert: trait traitComposition innerClass equals: TViewModelMock.
	self assert: trait package name equals: '_UnpackagedPackage'
]
