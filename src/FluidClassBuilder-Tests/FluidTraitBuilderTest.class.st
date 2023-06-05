"
This class contains tests for `FluidTraitBuilder`
"
Class {
	#name : #FluidTraitBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'FluidClassBuilder-Tests-Base'
}

{ #category : #'private - cleanup' }
FluidTraitBuilderTest >> removeTestArtifactsFromSystem [

	self class environment
		at: #TMyClass
		ifPresent: [ :p | p removeFromSystem: false ]
		ifAbsent: [ ]
]

{ #category : #running }
FluidTraitBuilderTest >> setUp [

	super setUp.
	builder := FluidTraitBuilder new
]

{ #category : #running }
FluidTraitBuilderTest >> tearDown [

	self class environment
		at: #TTestTrait
		ifPresent: [ :cl | cl removeFromSystem  ].
	super tearDown
]

{ #category : #'tests - classBuilder generation' }
FluidTraitBuilderTest >> testBuildSimplePoint2 [

	| trait |
	builder := Trait << #TPoint2
					slots: { #x . #y };
					package: self packageNameForTest.
	trait := builder build.

	self assert: trait name equals: #TPoint2.
	self assert: trait slots size equals: 2
]

{ #category : #'class side' }
FluidTraitBuilderTest >> testClassSlots [

	builder classSlots: {#string . #runs}.
	self assert: builder classSlotsToBuild equals: {#string => InstanceVariableSlot. #runs => InstanceVariableSlot}
]

{ #category : #'tests - class creation' }
FluidTraitBuilderTest >> testCreatingEmptyTraitHasDefaultElements [

	| instBuilder newTrait |
	instBuilder := (Trait << #TTestTrait
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newTrait := instBuilder install.

	self assert: newTrait name equals: #TTestTrait.
	self assert: newTrait slotNames equals: #().
	self assert: newTrait traitComposition equals: {} asTraitComposition.
	self assert: newTrait class traitComposition equals: {} asTraitComposition.
	self assert: newTrait classVarNames equals: #().
	self assert: newTrait category equals: 'Mock'
]

{ #category : #'tests - class creation' }
FluidTraitBuilderTest >> testCreatingFullTraitHasAllElements [

	| instBuilder newTrait |
	instBuilder := (Trait << #TTestTrait
		slots: #(a b c);
		traits: {TViewModelMock};
		tag: 'lala';
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newTrait := instBuilder install.

	self assert: newTrait name equals: #TTestTrait.
	self assert: newTrait slotNames equals: #(a b c).
	self assert: newTrait traitComposition equals: {TViewModelMock} asTraitComposition.
	self assert: newTrait class traitComposition equals: {TViewModelMock classSide} asTraitComposition.
	self assert: newTrait classVarNames equals: #().
	self assert: newTrait category equals: 'Mock-lala'
]

{ #category : #'tests - class creation' }
FluidTraitBuilderTest >> testExistingTraitWithClassSlotsArePreservedIfChangingInstanceSide [

	| instBuilder newTrait |
	instBuilder := (Trait << #TTestTrait
		classSlots: #(AAA);
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newTrait := instBuilder install.

	instBuilder := (Trait << #TTestTrait
		slots: #(aaa);
		package: 'Mock').

	newTrait := instBuilder install.

	self assert: newTrait class slotNames equals: #(AAA)
]

{ #category : #'tests - class creation' }
FluidTraitBuilderTest >> testExistingTraitWithSlotsArePreservedIfChangingClassSide [

	| instBuilder newTrait classBuilder |
	instBuilder := (Trait << #TTestTrait
		slots: #(aaa);
		package: 'Mock').

	"Ideally we should not install the class but just build it (sending instBuilder build)
	the problem is that category and tag are lost by the class builder.
	Hence we cannot test for real."
	newTrait := instBuilder install.

	classBuilder := (Trait << newTrait classTrait
		slots: #(AAA);
		package: 'Mock').

	newTrait := classBuilder install.

	self assert: newTrait class slotNames equals: #(AAA)	.
	self assert: newTrait slotNames equals: #(aaa)
]

{ #category : #'tests - classBuilder generation' }
FluidTraitBuilderTest >> testFillShiftClassBuilder [

	| shift |
	builder := Trait << #TPoint2.
	builder slots: { #x. #y }.
	builder traits: { TViewModelMock }.
	builder tag: 'Mafia'.
	builder package: self packageNameForTest.
	builder fillShiftClassBuilder.
	shift := builder shiftClassBuilder.
	self assert: shift name equals: #TPoint2.
	self assert: shift slots size equals: 2.
	self assert: shift category equals: self packageNameForTest , '-Mafia'.
	self flag: #askPablo
	"self assert: clas package packageName equals: self packageNameForTest"
]

{ #category : #'tests - install' }
FluidTraitBuilderTest >> testInstallMinimalMockClass [

	| shiftClassBuilder installedClass |
	self removeTestArtifactsFromSystem.
	[
	builder := self class compilerClass new
		           evaluate: 'Trait << #TMyClass
	traits: {};
	slots: {};
	tag: '''' ;
	package: ''MyPackage'''.
	shiftClassBuilder := builder shiftClassBuilder.
	ShiftClassInstaller new makeWithBuilder: shiftClassBuilder.

	installedClass := self class environment at: #TMyClass ifAbsent: [self fail].

	self assert: installedClass name equals: #TMyClass.
	self assert: installedClass isFixed.
	self assert: installedClass slots isEmpty.
	self assert: installedClass traitComposition isEmpty ]
		ensure: [ self removeTestArtifactsFromSystem ]
]

{ #category : #'tests - install' }
FluidTraitBuilderTest >> testInstallSimplePoint2 [

	[
	| trait |
	self assert: (self class environment at: #TPoint2 ifAbsent: [ true ]).

	builder := (Trait << #TPoint2)
		           slots: { #x. #y };
		           package: self packageNameForTest.
	builder install.
	trait := self class environment at: #TPoint2.
	self assert: trait name equals: #TPoint2.
	self assert: trait slots size equals: 2 ] ensure: [
		self class environment removeKey: #TPoint2 ifAbsent: [ self fail ].
		self assert: (self class environment at: #TPoint2 ifAbsent: [ true ]) ]
]

{ #category : #tests }
FluidTraitBuilderTest >> testTraitNoSlotsNoUse [

	| trait |
	builder := Trait << #TPoint3
						package: 'Fake'.

	trait := builder build.

	self assert: trait name equals: #TPoint3.
	self assert: trait package packageName equals: '_UnpackagedPackage'
]

{ #category : #tests }
FluidTraitBuilderTest >> testTraitUse [

	| trait |
	builder := (Trait << #TPoint3)
		           traits: TViewModelMock;
		           package: 'Fake'.
	trait := builder build.

	self assert: trait name equals: #TPoint3.
	self assert: trait traitComposition innerClass equals: TViewModelMock.
	self assert: trait package packageName equals: '_UnpackagedPackage'
]
