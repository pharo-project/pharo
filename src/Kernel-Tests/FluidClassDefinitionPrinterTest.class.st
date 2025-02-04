Class {
	#name : 'FluidClassDefinitionPrinterTest',
	#superclass : 'CommonClassDefinitionPrinterTest',
	#category : 'Kernel-Tests-Fluid',
	#package : 'Kernel-Tests',
	#tag : 'Fluid'
}

{ #category : 'helpers' }
FluidClassDefinitionPrinterTest >> expandedForClass: aClass [

	^ aClass expandedDefinitionStringFor: (FluidClassDefinitionPrinter for: aClass)
]

{ #category : 'helpers' }
FluidClassDefinitionPrinterTest >> forClass: aClass [

	^ (ClassDefinitionPrinter fluid for: aClass) definitionString
]

{ #category : 'tests - classes' }
FluidClassDefinitionPrinterTest >> testArray [

	self
		assert: (self forClass: Array)
		equals: 'ArrayedCollection << #Array
	layout: VariableLayout;
	tag: ''Base'';
	package: ''Collections-Sequenceable'''
]

{ #category : 'tests - classes' }
FluidClassDefinitionPrinterTest >> testArrayedCollectionWithPoolDictionary [

	self assert: (self forClass: ExampleForTestWithSharedPool) equals: 'Object << #ExampleForTestWithSharedPool
	sharedPools: { PoolDefiner };
	tag: ''Classes'';
	package: ''Kernel-Tests'''
]

{ #category : 'tests - classes' }
FluidClassDefinitionPrinterTest >> testByteString [

	self assert: (self forClass: ByteString) equals: 'String << #ByteString
	layout: ByteLayout;
	sharedVariables: { #NonAsciiMap };
	tag: ''Base'';
	package: ''Collections-Strings'''
]

{ #category : 'tests - classes' }
FluidClassDefinitionPrinterTest >> testByteStringClass [

	self
		assert: (self forClass: ByteString class)
		equals: 'String class << ByteString class'
]

{ #category : 'tests - classes' }
FluidClassDefinitionPrinterTest >> testClassPool [

	self
		assert: (self forClass: PoolDefiner)
		equals: 'SharedPool << #PoolDefiner
	sharedVariables: { #AnAuthor . #Gloups };
	tag: ''Classes'';
	package: ''Kernel-Tests'''
]

{ #category : 'tests - template' }
FluidClassDefinitionPrinterTest >> testCompactClassTemplate [

	self assert: (FluidClassDefinitionPrinter new compactClassDefinitionTemplateInPackage: 'Kernel' tag: #Basic) equals: 'Object << #MyClass
	slots: {};
	tag: #Basic;
	package: ''Kernel'''
]

{ #category : 'tests - complex slots' }
FluidClassDefinitionPrinterTest >> testDefinitionWithComplexSlot [
	"Fluid definition support complex slots."

	self assert: (self forClass: MockWithComplexSlot) equals: 'Object << #MockWithComplexSlot
	slots: { #aComplexSlot => ObservableSlot };
	tag: ''Fluid'';
	package: ''Kernel-Tests'''
]

{ #category : 'tests - unnecessary values' }
FluidClassDefinitionPrinterTest >> testDisplayEmptySlots [
	"I would like to be able to test if I provide too many empty values that
	the default empty values are just ignored."
	| class |
	ClassDefinitionPrinter displayEmptySlots: true.
	class := (Object << #CDFluidMock
	tag: 'BasicObjects';
	package: 'Kernel') build.
	self assert: (self forClass: class) equals: 'Object << #CDFluidMock
	slots: {};
	package: ''_UnpackagedPackage'''
]

{ #category : 'tests - unnecessary values' }
FluidClassDefinitionPrinterTest >> testDoesNotDisplayEmptySlots [

	| class |
	ClassDefinitionPrinter displayEmptySlots: false.
	class := (Object << #CDFluidMock
	tag: 'BasicObjects';
	package: 'Kernel') build.
	self assert: (self forClass: class) equals: 'Object << #CDFluidMock
	package: ''_UnpackagedPackage'''
]

{ #category : 'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedEmptyLayoutClass [

	self
		assert: (self expandedForClass: EmptyLayout class)
		equals: 'AbstractLayout class << EmptyLayout class
	traits: {};
	slots: { #instance }'
]

{ #category : 'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedOnClassWithoutTag [

	self
		assert: (self expandedForClass: EncoderForSistaV1)
		equals:   'BytecodeEncoder << #EncoderForSistaV1
	layout: FixedLayout;
	traits: {};
	slots: {};
	sharedVariables: {};
	sharedPools: {};
	tag: '''';
	package: ''Kernel-BytecodeEncoders'''
]

{ #category : 'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedPoint [

	self
		assert: (self expandedForClass: Point)
		equals: 'Object << #Point
	layout: FixedLayout;
	traits: {};
	slots: { #x . #y };
	sharedVariables: {};
	sharedPools: {};
	tag: ''BasicObjects'';
	package: ''Kernel'''
]

{ #category : 'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedPointClass [

	self
		assert: (self expandedForClass: Point class)
		equals: 'Object class << Point class
	traits: {};
	slots: {}'
]

{ #category : 'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedProtoObject [

	self
		assert: (self expandedForClass: ProtoObject)
		equals: 'nil << #ProtoObject
	layout: FixedLayout;
	traits: {};
	slots: {};
	sharedVariables: {};
	sharedPools: {};
	tag: ''Objects'';
	package: ''Kernel'''
]

{ #category : 'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedProtoObjectClass [

	self
		assert: (self expandedForClass: ProtoObject class)
		equals: 'Class class << ProtoObject class
	traits: {};
	slots: {}'
]

{ #category : 'tests - expanded' }
FluidClassDefinitionPrinterTest >> testExpandedTime [

	self
		assert: (self expandedForClass: RootClassPoolUser)
		equals: 'Object << #RootClassPoolUser
	layout: FixedLayout;
	traits: {};
	slots: {};
	sharedVariables: {};
	sharedPools: { PoolDefiner };
	tag: ''Classes'';
	package: ''Kernel-Tests'''
]

{ #category : 'tests - template' }
FluidClassDefinitionPrinterTest >> testFullClassTemplate [

	self assert: (FluidClassDefinitionPrinter new classDefinitionTemplateInPackage: 'Kernel' tag: nil named: #MyClass) equals: 'Object << #MyClass
	layout: FixedLayout;
	traits: {};
	slots: {};
	sharedVariables: {};
	sharedPools: {};
	tag: '''' ;
	package: ''Kernel'''
]

{ #category : 'tests - metaclasses' }
FluidClassDefinitionPrinterTest >> testMetaclass [

	self assert: (self forClass: Metaclass) equals: 'ClassDescription << #Metaclass
	slots: { #thisClass };
	tag: ''Classes'';
	package: ''Kernel-CodeModel'''
]

{ #category : 'tests - metaclasses' }
FluidClassDefinitionPrinterTest >> testMetaclassClass [

	self assert: (self forClass: Metaclass class) equals:
 'ClassDescription class << Metaclass class'
]

{ #category : 'tests - classes' }
FluidClassDefinitionPrinterTest >> testPoint [

	self assert: (self forClass: Point) equals: 'Object << #Point
	slots: { #x . #y };
	tag: ''BasicObjects'';
	package: ''Kernel'''
]

{ #category : 'tests - unnecessary values' }
FluidClassDefinitionPrinterTest >> testPointMinimalInformation [
	"I would like to be able to test if I provide too many empty values that
	the default empty values are just ignored."
	| class |
	class := (Object << #CDFluidMock
	slots: {#x . #y};
	tag: 'BasicObjects';
	package: 'Kernel') build.
	"I cannot test the tag because the class is not installed so the package manager
	does not know the package and the class."
	self assert: (self forClass: class) equals: 'Object << #CDFluidMock
	slots: { #x . #y };
	package: ''_UnpackagedPackage'''
]

{ #category : 'tests - classes' }
FluidClassDefinitionPrinterTest >> testProtoObject [

	self assert: (self forClass: ProtoObject) equals: 'nil << #ProtoObject
	tag: ''Objects'';
	package: ''Kernel'''
]

{ #category : 'tests - classes' }
FluidClassDefinitionPrinterTest >> testProtoObjectClass [

	self assert: (self forClass: ProtoObject class) equals: 'Class class << ProtoObject class'
]

{ #category : 'tests - metaclasses' }
FluidClassDefinitionPrinterTest >> testSystemAnnouncerClass [

	self assert: (self forClass: SystemAnnouncer class) equals: 'Announcer class << SystemAnnouncer class
	slots: { #announcer }'
]
