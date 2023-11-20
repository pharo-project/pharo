Class {
	#name : 'TraitFluidClassDefinitionPrinterTest',
	#superclass : 'TraitCommonClassDefinitionPrinterTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'helpers' }
TraitFluidClassDefinitionPrinterTest >> expandedForClass: aClass [

	^ aClass expandedDefinitionStringFor: (FluidClassDefinitionPrinter for: aClass)
]

{ #category : 'helpers' }
TraitFluidClassDefinitionPrinterTest >> forClass: aClass [
	^ (FluidClassDefinitionPrinter for: aClass) definitionString
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testAlignmentMorphClass [

	self
		assert: (self forClass: AlignmentMorph class)
		equals: 'BorderedMorph class << AlignmentMorph class
	traits: {TAbleToRotate classTrait}'
]

{ #category : 'tests - metaclasses' }
TraitFluidClassDefinitionPrinterTest >> testClassSideDoesNotShowPackage [

	self assert: (self forClass: MOPTraitTest class) equals: 'TestCase class << MOPTraitTest class'
]

{ #category : 'tests - template' }
TraitFluidClassDefinitionPrinterTest >> testCompactTraitFullTemplate [

	self assert: (FluidClassDefinitionPrinter new compactTraitDefinitionTemplateInPackage: 'Traits' tag: #Basic) equals: 'Trait << #TMyTrait
	traits: {};
	slots: {};
	tag: #Basic;
	package: ''Traits'''
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testComplexTrait [

	self assert: (self forClass: MOPTestClassD) equals: 'Object << #MOPTestClassD
	traits: {Trait2 @ {#c3->#c2}};
	tag: ''MOP'';
	package: ''Traits-Tests'''
]

{ #category : 'tests - complex slots' }
TraitFluidClassDefinitionPrinterTest >> testDefinitionWithComplexSlot [
	"Fluid definition support complex slots."

	self assert: (self forClass: TMockWithComplexSlot) equals: 'Trait << #TMockWithComplexSlot
	slots: { #complexSlot => ObservableSlot };
	package: ''Traits-Tests'''
]

{ #category : 'tests - expanded' }
TraitFluidClassDefinitionPrinterTest >> testExpandedClySubclassWithTraitsOfClassWithTraits [

	self
		assert: (self expandedForClass: ClySubclassWithTraitsOfClassWithTraits)
		equals: 'ClyClassWithTraits << #ClySubclassWithTraitsOfClassWithTraits
	layout: FixedLayout;
	traits: {ClyTraitChild3};
	slots: {};
	sharedVariables: {};
	sharedPools: {};
	tag: '''';
	package: ''Calypso-SystemPlugins-Traits-Queries-Tests-PWithTraits'''
]

{ #category : 'tests - expanded' }
TraitFluidClassDefinitionPrinterTest >> testExpandedEpTEventVisitorClassTrait [

	self
		assert: (self expandedForClass: EpTEventVisitor classTrait)
		equals: 'Trait << EpTEventVisitor classTrait
	traits: {EpTCodeChangeVisitor classTrait};
	slots: {}'
]

{ #category : 'tests - expanded' }
TraitFluidClassDefinitionPrinterTest >> testExpandedEpTEventVisitorTrait [

	self
		assert: (self expandedForClass: EpTEventVisitor)
		equals: 'Trait << #EpTEventVisitor
	traits: {EpTCodeChangeVisitor};
	slots: {};
	tag: ''Visitors'';
	package: ''Epicea'''
]

{ #category : 'tests - expanded' }
TraitFluidClassDefinitionPrinterTest >> testExpandedMetaclassWithTrait [

	self assert: (self expandedForClass: MOPTestClassD class) equals: 'Object class << MOPTestClassD class
	traits: {Trait2 classTrait};
	slots: {}'
]

{ #category : 'tests - expanded' }
TraitFluidClassDefinitionPrinterTest >> testExpandedTComparableClassTrait [

	self
		assert:  (self expandedForClass: TComparable classTrait)
		equals:  'Trait << TComparable classTrait
	traits: {};
	slots: {}'
]

{ #category : 'tests - expanded' }
TraitFluidClassDefinitionPrinterTest >> testExpandedTrait3ClassTrait [

	self
		assert: (self expandedForClass: Trait3 classTrait)
		equals:  'Trait << Trait3 classTrait
	traits: {Trait2 classTrait};
	slots: {}'
]

{ #category : 'tests - template' }
TraitFluidClassDefinitionPrinterTest >> testFullTraitTemplate [

	self
		assert: (FluidClassDefinitionPrinter new traitDefinitionTemplateInPackage: 'Traits') equals: 'Trait << #TMyTrait
	traits: {};
	slots: {};
	tag: '''' ;
	package: ''Traits'''
]

{ #category : 'tests - metaclasses' }
TraitFluidClassDefinitionPrinterTest >> testMetaclassWithTrait [

	self assert: (self forClass: MOPTestClassD class) equals: 'Object class << MOPTestClassD class
	traits: {Trait2 classTrait}'
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testMultiPools [

	self assert: (self forClass: ClassMultiplePoolUser) equals: 'Object << #ClassMultiplePoolUser
	sharedPools: { PoolDefiner . PoolDefiner2 };
	tag: ''Classes'';
	package: ''Kernel-Tests'''
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testTComparableClassTrait [

	self assert: (self forClass: TComparable classTrait) equals: 'Trait << TComparable classTrait'
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testTEventVisitorClassTrait [

	self
		assert: (self forClass: EpTEventVisitor classTrait)
		equals: 'Trait << EpTEventVisitor classTrait
	traits: {EpTCodeChangeVisitor classTrait}'
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testTIsEmpty [

	self
		assert: (self forClass: TIsEmpty)
		equals: 'Trait << #TIsEmpty
	tag: ''Base'';
	package: ''Kernel-Traits'''
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testTSlot [

	self
		assert: (Trait << #TTUses slots: {  #x . #y }; package: 'Fake') build definitionString
		equals:
	'Trait << #TTUses
	slots: { #x . #y };
	package: ''_UnpackagedPackage'''
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testTUses [

	self
		assert: ((Trait << #TTUses)
				 traits: TEmpty;
				 package: 'Fake') build definitionString
		equals: 'Trait << #TTUses
	traits: {TEmpty};
	package: ''_UnpackagedPackage'''
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testTrait [

	self assert: (self forClass: RubScrolledTextModel) equals: 'Object << #RubScrolledTextModel
	traits: {TViewModel};
	slots: { #hasUnacceptedEdits . #announcer . #text . #primarySelectionInterval . #interactionModel };
	tag: ''Editing-Widgets'';
	package: ''Rubric'''
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testTrait3 [

	self assert: (self forClass: Trait3 classTrait) equals: 'Trait << Trait3 classTrait
	traits: {Trait2 classTrait}'
]

{ #category : 'tests - traits' }
TraitFluidClassDefinitionPrinterTest >> testTrait3AndTag [

	self assert: (self forClass: Trait3) equals: 'Trait << #Trait3
	traits: {Trait2};
	tag: ''MOP'';
	package: ''Traits-Tests'''
]
