Class {
	#name : #FluidClassDefinitionPrinterTest,
	#superclass : #CommonClassDefinitionPrinterTest,
	#category : #'Kernel-Tests-Fluid'
}

{ #category : #helpers }
FluidClassDefinitionPrinterTest >> expandedForClass: aClass [
	^ (FluidClassDefinitionPrinter for: aClass) expandedDefinitionString 


]

{ #category : #helpers }
FluidClassDefinitionPrinterTest >> forClass: aClass [
	^ (FluidClassDefinitionPrinter for: aClass) definitionString 


]

{ #category : #running }
FluidClassDefinitionPrinterTest >> setUp [

	super setUp.
	ClassDefinitionPrinter showFluidClassDefinition: true
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testAlignmentMorphClass [

	self 
		assert: (self forClass: AlignmentMorph class) 
		equals: 'AlignmentMorph class
	uses: TAbleToRotate classTrait' 
]

{ #category : #'tests - classes' }
FluidClassDefinitionPrinterTest >> testArray [

	self 
		assert: (self forClass: Array)
		equals: 'ArrayedCollection < #Array
	layout: VariableLayout;
	tag: ''Base'';
	package: ''Collections-Sequenceable'''
]

{ #category : #'tests - classes' }
FluidClassDefinitionPrinterTest >> testByteString [

	self assert: (self forClass: ByteString) equals: 'String < #ByteString
	layout: ByteLayout;
	sharedVariables: { #NonAsciiMap };
	tag: ''Base'';
	package: ''Collections-Strings'''  
]

{ #category : #'tests - classes' }
FluidClassDefinitionPrinterTest >> testChronologyConstants [
	
	self assert: (self forClass: ChronologyConstants) equals: 'SharedPool < #ChronologyConstants
	sharedVariables: { #NanosInSecond . #MonthNames . #SecondsInHour . #SecondsInDay . #SqueakEpoch . #NanosInMillisecond . #DayNames . #DaysInMonth . #SecondsInMinute . #MicrosecondsInDay };
	tag: ''Chronology'';
	package: ''Kernel'''
]

{ #category : #'tests - metaclasses' }
FluidClassDefinitionPrinterTest >> testClassSideDoesNotShowPackage [

	self assert: (self forClass: MOPTraitTest class) equals: 'MOPTraitTest class'
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testComplexTrait [

	self assert: (self forClass: MOPTestClassD) equals: 'Object < #MOPTestClassD
	uses: Trait2 @ {#c3->#c2};
	tag: ''Traits-MOP'';
	package: ''Tests''' 
]

{ #category : #'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedEmptyLayoutClass [

	self 
		assert: (self expandedForClass: EmptyLayout class)  
		equals: 'EmptyLayout class
	uses: {};
	slots: { #instance }'
]

{ #category : #'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedEpTEventVisitorClassTrait [

	self 
		assert: (self expandedForClass: EpTEventVisitor classTrait)   
		equals: 'EpTEventVisitor classTrait
	uses: EpTCodeChangeVisitor classTrait;
	slots: {}'
]

{ #category : #'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedMetaclassWithTrait [
	
	self assert: (self expandedForClass: MOPTestClassD class) equals: 'MOPTestClassD class
	uses: Trait2 classTrait;
	slots: {}' 
]

{ #category : #'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedOnClassWithoutTag [

	self 
		assert: (self expandedForClass: EncoderForV3)
		equals:   'Object < #EncoderForV3
	layout: FixedLayout;
	uses: {};
	slots: {};
	sharedVariables: {};
	sharedPools: {};
	tag: '''';
	package: ''Kernel-BytecodeEncoders'''
]

{ #category : #'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedPoint [

	self 
		assert: (self expandedForClass: Point) 
		equals: 'Object < #Point
	layout: FixedLayout;
	uses: {};
	slots: { #x . #y };
	sharedVariables: {};
	sharedPools: {};
	tag: ''BasicObjects'';
	package: ''Kernel''' 
	
]

{ #category : #'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedTComparableClassTrait [

	self 
		assert:  (self expandedForClass: TComparable classTrait) 
		equals:  'TComparable classTrait
	uses: {};
	slots: {}'
	
]

{ #category : #'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedTime [

	self 
		assert: (self expandedForClass: Time)  
		equals: 'Object < #Time
	layout: FixedLayout;
	uses: {};
	slots: { #seconds . #nanos };
	sharedVariables: {};
	sharedPools: { ChronologyConstants };
	tag: ''Chronology'';
	package: ''Kernel'''
]

{ #category : #'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedTrait3ClassTrait [

	self 
		assert: (self expandedForClass: Trait3 classTrait) 
		equals:  'Trait3 classTrait
	uses: Trait2 classTrait;
	slots: {}'
		
]

{ #category : #'tests - fully expanded' }
FluidClassDefinitionPrinterTest >> testFullyExpandedSimple [

	self 
		assert: (FluidClassDefinitionPrinter new classDefinitionTemplateInPackage: 'Kernel') 
		equals:  'Object < #MyClass
	layout: FixedLayout;
	uses: {};
	slots: {};
	sharedVariables: {};
	sharedPools: {};
	tag: '''' ;
	package: ''Kernel'''
		
]

{ #category : #'tests - fully expanded' }
FluidClassDefinitionPrinterTest >> testFullyExpandedTrait [

	self 
		assert: (FluidClassDefinitionPrinter new traitDefinitionTemplateInPackage: 'TraitsV2') equals: 'Trait << #TMyTrait
	uses: {};
	slots: {};
	tag: '''' ;
	package: ''TraitsV2'''
]

{ #category : #'tests - metaclasses' }
FluidClassDefinitionPrinterTest >> testMetaclass [
	
	self assert: (self forClass: Metaclass) equals: 'ClassDescription < #Metaclass
	slots: { #thisClass };
	tag: ''Classes'';
	package: ''Kernel''' 
]

{ #category : #'tests - metaclasses' }
FluidClassDefinitionPrinterTest >> testMetaclassClass [
	
	self assert: (self forClass: Metaclass class) equals: 
 'Metaclass class'	
]

{ #category : #'tests - metaclasses' }
FluidClassDefinitionPrinterTest >> testMetaclassWithTrait [
	
	self assert: (self forClass: MOPTestClassD class) equals: 'MOPTestClassD class
	uses: Trait2 classTrait' 
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testMultiPools [

	self assert: (self forClass: ClassMultiplePoolUser) equals: 'Object < #ClassMultiplePoolUser
	sharedPools: { PoolDefiner . PoolDefiner2 };
	tag: ''Classes'';
	package: ''Kernel-Tests''' 
]

{ #category : #'tests - classes' }
FluidClassDefinitionPrinterTest >> testPoint [
	
	self assert: (self forClass: Point) equals: 'Object < #Point
	slots: { #x . #y };
	tag: ''BasicObjects'';
	package: ''Kernel'''
	
]

{ #category : #'tests - unnecessary values' }
FluidClassDefinitionPrinterTest >> testPointMinimalInformation [
	"I would like to be able to test if I provide too many empty values that
	the default empty values are just ignored."
	| class |
	class := (Object < #CDFluidMock
	slots: {#x . #y};
	tag: 'BasicObjects';
	package: 'Kernel') build.
	"I cannot test the tag because the class is not installed so the package manager
	does not know the package and the class."
	self assert: (self forClass: class) equals: 'Object < #CDFluidMock
	slots: { #x . #y };
	package: ''_UnpackagedPackage'''
	
]

{ #category : #'tests - classes' }
FluidClassDefinitionPrinterTest >> testProtoObject [
	
	self assert: (self forClass: ProtoObject) equals: 'nil < #ProtoObject
	tag: ''Objects'';
	package: ''Kernel''' 
]

{ #category : #'tests - classes' }
FluidClassDefinitionPrinterTest >> testProtoObjectClass [
	
	self assert: (self forClass: ProtoObject class) equals: 'ProtoObject class'
]

{ #category : #'tests - metaclasses' }
FluidClassDefinitionPrinterTest >> testSystemAnnouncerClass [
	
	self assert: (self forClass: SystemAnnouncer class) equals: 'SystemAnnouncer class
	slots: { #announcer }'
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testTBehavior [

	self 
		assert: (self forClass: TBehavior) 
		equals: 'Trait  << #TBehavior
	tag: ''Traits'';
	package: ''TraitsV2-Compatibility'''
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testTComparableClassTrait [

	self assert: (self forClass: TComparable classTrait) equals: 'TComparable classTrait' 
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testTEventVisitorClassTrait [

	self assert: (self forClass: EpTEventVisitor classTrait) equals: 'EpTEventVisitor classTrait
	uses: EpTCodeChangeVisitor classTrait'
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testTSlot [

	self 
		assert: (Trait << #TTUses slots: {  #x . #y }; package: 'Fake') build definitionString 
		equals: 
	'Trait  << #TTUses
	slots: { #x . #y };
	package: ''_UnpackagedPackage''' 
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testTUses [

	self assert: (Trait << #TTUses uses: TBehavior; package: 'Fake') build definitionString equals:  'Trait  << #TTUses
	uses: TBehavior;
	package: ''_UnpackagedPackage'''
]

{ #category : #'tests - classes' }
FluidClassDefinitionPrinterTest >> testText [

	self assert: (self forClass: Text) equals: 'ArrayedCollection < #Text
	slots: { #string . #runs };
	sharedPools: { TextConstants };
	tag: ''Base'';
	package: ''Text-Core''' 
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testTrait [
	
	self assert: (self forClass: RubScrolledTextModel) equals: 'Object < #RubScrolledTextModel
	uses: TViewModel;
	slots: { #hasUnacceptedEdits . #announcer . #text . #primarySelectionInterval . #interactionModel };
	tag: ''Editing-Widgets'';
	package: ''Rubric''' 
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testTrait3 [

	self assert: (self forClass: Trait3 classTrait) equals: 'Trait3 classTrait
	uses: Trait2 classTrait'
]

{ #category : #'tests - traits' }
FluidClassDefinitionPrinterTest >> testTrait3AndTag [

	self 
		assert: (self forClass: Trait3) 
		equals: 'Trait  << #Trait3
	uses: Trait2;
	tag: ''Traits-MOP'';
	package: ''Tests''' 
]
