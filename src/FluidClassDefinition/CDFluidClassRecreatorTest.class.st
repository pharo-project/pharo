"
This test suite is about recreating a class textual representation from the class.
"
Class {
	#name : #CDFluidClassRecreatorTest,
	#superclass : #TestCase,
	#instVars : [
		'classDefinition',
		'slotuse'
	],
	#category : #'FluidClassDefinition-Parser'
}

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> setUp [ 
	
	slotuse := Slot showSlotClassDefinition.
	Slot showSlotClassDefinition: true.
	super setUp.
	
]

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> tearDown [ 
	
	Slot showSlotClassDefinition: slotuse.
	super tearDown.
	
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testAlignmentMorphClass [

	self 
		assert: AlignmentMorph class asFluidDefinitionString 
		equals: 'AlignmentMorph class
	uses: TAbleToRotate classTrait' 
]

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> testArray [

	self assert: Array asFluidDefinitionString equals: 'ArrayedCollection < #Array
	layout: VariableLayout;
	tag: ''Base'';
	package: ''Collections-Sequenceable'''
]

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> testByteString [

	self assert: ByteString asFluidDefinitionString equals: 'String < #ByteString
	layout: ByteLayout;
	sharedVariables: { #NonAsciiMap };
	tag: ''Base'';
	package: ''Collections-Strings'''  
]

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> testChronologyConstants [
	
	self assert: ChronologyConstants asFluidDefinitionString equals: 'SharedPool < #ChronologyConstants
	sharedVariables: { #NanosInSecond . #MonthNames . #SecondsInHour . #SecondsInDay . #SqueakEpoch . #NanosInMillisecond . #DayNames . #DaysInMonth . #SecondsInMinute . #MicrosecondsInDay };
	tag: ''Chronology'';
	package: ''Kernel'''
]

{ #category : #'tests - metaclasses' }
CDFluidClassRecreatorTest >> testClassSideDoesNotShowPackage [

	self assert: MOPTraitTest class asFluidDefinitionString  equals: 'MOPTraitTest class'
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testComplexTrait [

	self assert: MOPTestClassD asFluidDefinitionString equals: 'Object < #MOPTestClassD
	uses: Trait2 @ {#c3->#c2};
	tag: ''Traits-MOP'';
	package: ''Tests''' 
]

{ #category : #'tests - expanded' }
CDFluidClassRecreatorTest >> testExpandedEmptyLayoutClass [

	self 
		assert: EmptyLayout class definitionExpanded  
		equals: 'EmptyLayout class
	uses: {};
	slots: { #instance }'
]

{ #category : #'tests - expanded' }
CDFluidClassRecreatorTest >> testExpandedEpTEventVisitorClassTrait [

	self 
		assert: EpTEventVisitor classTrait definitionExpanded   
		equals: 'EpTEventVisitor classTrait
	uses: EpTCodeChangeVisitor classTrait;
	slots: {}'
]

{ #category : #'tests - expanded' }
CDFluidClassRecreatorTest >> testExpandedMetaclassWithTrait [
	
	self assert: MOPTestClassD class definitionExpanded equals: 'MOPTestClassD class
	uses: Trait2 classTrait;
	slots: {}' 
]

{ #category : #'tests - expanded' }
CDFluidClassRecreatorTest >> testExpandedPoint [

	self 
		assert: Point definitionExpanded 
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
CDFluidClassRecreatorTest >> testExpandedTComparableClassTrait [

	self 
		assert:  TComparable classTrait definitionExpanded 
		equals:  'TComparable classTrait
	uses: {};
	slots: {}'
	
]

{ #category : #'tests - expanded' }
CDFluidClassRecreatorTest >> testExpandedTime [

	self 
		assert: Time definitionExpanded  
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
CDFluidClassRecreatorTest >> testExpandedTrait3ClassTrait [

	self 
		assert: Trait3 classTrait definitionExpanded 
		equals:  'Trait3 classTrait
	uses: Trait2 classTrait;
	slots: {}'
		
]

{ #category : #'tests - expanded' }
CDFluidClassRecreatorTest >> testFullyExpandedSimple [

	self 
		assert: ClassDescription definitionFullyExpanded 
		equals:  'Object < #MyClass
	layout: FixedLayout;
	uses: {};
	slots: {};
	sharedVariables: {};
	sharedPools: {};
	tag: '''' ;
	package: ''MyPackage'''
		
]

{ #category : #'tests - expanded' }
CDFluidClassRecreatorTest >> testFullyExpandedTrait [

	self 
		assert: Trait definitionFullyExpanded equals: 'Trait << #TMyTrait
	uses: {};
	slots: {};
	tag: '''' ;
	package: ''MyPackage'''
]

{ #category : #'tests - metaclasses' }
CDFluidClassRecreatorTest >> testMetaclass [
	
	self assert: Metaclass asFluidDefinitionString equals: 'ClassDescription < #Metaclass
	slots: { #thisClass };
	tag: ''Classes'';
	package: ''Kernel''' 
]

{ #category : #'tests - metaclasses' }
CDFluidClassRecreatorTest >> testMetaclassClass [
	
	self assert: Metaclass class asFluidDefinitionString equals: 
 'Metaclass class'	
]

{ #category : #'tests - metaclasses' }
CDFluidClassRecreatorTest >> testMetaclassWithTrait [
	
	self assert: MOPTestClassD class asFluidDefinitionString equals: 'MOPTestClassD class
	uses: Trait2 classTrait' 
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testMultiPools [

	self assert: ClassMultiplePoolUser asFluidDefinitionString equals: 'Object < #ClassMultiplePoolUser
	sharedPools: { PoolDefiner . PoolDefiner2 };
	tag: ''Classes'';
	package: ''Kernel-Tests''' 
]

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> testPoint [
	
	self assert: Point asFluidDefinitionString equals: 'Object < #Point
	slots: { #x . #y };
	tag: ''BasicObjects'';
	package: ''Kernel'''
	
]

{ #category : #'tests - unnecessary values' }
CDFluidClassRecreatorTest >> testPointMinimalInformation [
	"I would like to be able to test if I provide too many empty values that
	the default empty values are just ignored."
	| class |
	class := (Object < #CDFluidMock
	slots: {#x . #y};
	tag: 'BasicObjects';
	package: 'Kernel') build.
	"I cannot test the tag because the class is not installed so the package manager
	does not know the package and the class."
	self assert: class asFluidDefinitionString equals: 'Object < #CDFluidMock
	slots: { #x . #y };
	package: ''_UnpackagedPackage'''
	
]

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> testProtoObject [
	
	self assert: ProtoObject asFluidDefinitionString equals: 'nil < #ProtoObject
	tag: ''Objects'';
	package: ''Kernel''' 
]

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> testProtoObjectClass [
	
	self assert: ProtoObject class asFluidDefinitionString equals: 'ProtoObject class'
]

{ #category : #'tests - metaclasses' }
CDFluidClassRecreatorTest >> testSystemAnnouncerClass [
	
	self assert: SystemAnnouncer class asFluidDefinitionString equals: 'SystemAnnouncer class
	slots: { #announcer }'
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testTBehavior [

	self 
		assert: TBehavior asFluidDefinitionString 
		equals: 'Trait  << #TBehavior
	tag: ''Traits'';
	package: ''TraitsV2-Compatibility'''
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testTComparableClassTrait [

	self assert: TComparable classTrait asFluidDefinitionString equals: 'TComparable classTrait' 
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testTEventVisitorClassTrait [

	self assert: EpTEventVisitor classTrait asFluidDefinitionString equals: 'EpTEventVisitor classTrait
	uses: EpTCodeChangeVisitor classTrait'
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testTSlot [

	self 
		assert: (Trait << #TTUses slots: {  #x . #y }; package: 'Fake') build asFluidDefinitionString 
		equals: 
	'Trait  << #TTUses
	slots: { #x . #y };
	package: ''_UnpackagedPackage''' 
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testTUses [

	self assert: (Trait << #TTUses uses: TBehavior; package: 'Fake') build asFluidDefinitionString equals:  'Trait  << #TTUses
	uses: TBehavior;
	package: ''_UnpackagedPackage'''
]

{ #category : #'tests - classes' }
CDFluidClassRecreatorTest >> testText [

	self assert: Text asFluidDefinitionString equals: 'ArrayedCollection < #Text
	slots: { #string . #runs };
	sharedPools: { TextConstants };
	tag: ''Base'';
	package: ''Text-Core''' 
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testTrait [
	
	self assert: RubScrolledTextModel asFluidDefinitionString equals: 'Object < #RubScrolledTextModel
	uses: TViewModel;
	slots: { #hasUnacceptedEdits . #announcer . #text . #primarySelectionInterval . #interactionModel };
	tag: ''Editing-Widgets'';
	package: ''Rubric''' 
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testTrait3 [

	self assert: Trait3 classTrait asFluidDefinitionString equals: 'Trait3 classTrait
	uses: Trait2 classTrait'
]

{ #category : #'tests - traits' }
CDFluidClassRecreatorTest >> testTrait3AndTag [

	self 
		assert: Trait3 asFluidDefinitionString 
		equals: 'Trait  << #Trait3
	uses: Trait2;
	tag: ''Traits-MOP'';
	package: ''Tests''' 
]
