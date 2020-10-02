Class {
	#name : #FluidClassBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'FluidClassDefinition-Builder'
}

{ #category : #running }
FluidClassBuilderTest >> builder [ 	

	^  builder
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> removeFromSystem [

	self class environment at: #MyClass 
		ifPresent: [:p | p removeFromSystem: false ]
		ifAbsent: [  ] .
	
]

{ #category : #running }
FluidClassBuilderTest >> setUp [

	builder := FluidClassBuilder new 
		superclassToBuild: Object;
		classNameToBuild: #Point33;
		package: 'FakedPackage'

]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuildLayout [

	builder 
		layout: VariableLayout;
		package: 'Collections-Sequenceable'.
	self assert: builder layoutToBuild equals: VariableLayout

	
]

{ #category : #'tests - build' }
FluidClassBuilderTest >> testBuildSimplePoint2 [

	| clas |
	builder := Object < #Point2.
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
	self assert: builder sharedPoolsToBuild equals: 'TextConstants'.
	
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderSharedVariables [

	builder 
		layout: ByteLayout;
		sharedVariables: { #NonAsciiMap };
		package: 'Collections-Strings'.  
	self assert: builder sharedVariablesToBuild equals: { #NonAsciiMap }.
	self assert: builder layoutToBuild equals: ByteLayout.
	
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderTraits [
	
	builder uses: TViewModel.
	self assert: builder traitsToBuild equals: TViewModel 
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testBuilderWithPackage [

	builder package: 'Kernel-BasicObjects'.
	self assert: builder packageToBuild equals: 'Kernel-BasicObjects'

	
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderWithSlots [

	builder slots: { #x . #y }.
	self assert: builder slotsToBuild equals: { #x . #y }.

	
]

{ #category : #'tests - gathering' }
FluidClassBuilderTest >> testBuilderWithTag [

	builder tag: 'Foo'.
	self assert: builder tagToBuild equals: 'Foo'.

	
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testCreateBuilder [

	builder := Object < #Point.
	self assert: builder superclassToBuild equals: Object.
	self assert: builder classNameToBuild equals: #Point.
	
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testCreateBuilderWithNil [

	builder := nil < #NewProtoObject.
	self assert: builder superclassToBuild equals: nil.
	self assert: builder classNameToBuild equals: #NewProtoObject.
	
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testCreateClassWithFullExpandedDefinitionKeepsTheMinimum [

	"check ClassDescription>>#definitionFullExpanded"

	| shiftClassBuilder |
	builder := self class compilerClass new
		           evaluate: 'Object < #MyClass
	layout: FixedLayout;
	uses: {};
	slots: {};
	sharedVariables: {};
	tag: '''' ;
	sharedPools: {};
	package: ''MyPackage'''.
	shiftClassBuilder := builder shiftClassBuilder.
		          
	self assert: shiftClassBuilder superclass equals: Object.
	self assert: shiftClassBuilder name equals: #MyClass.
	self assert: shiftClassBuilder layoutDefinition slots isEmpty.
	"we should add slots to ShiftClassBuilder"
	self flag: #Pablo.
	self assert: shiftClassBuilder traitComposition isEmpty.
	self assert: shiftClassBuilder layoutDefinition sharedVariables isEmpty.
	self assert: shiftClassBuilder layoutDefinition sharedPools isEmpty.
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testInstallMinimalMockClass [

	| shiftClassBuilder installedClass |
	self removeFromSystem.
	[ 
	builder := self class compilerClass new
		           evaluate: 'Object < #MyClass
	layout: FixedLayout;
	uses: {};
	slots: {};
	sharedVariables: {};
	tag: '''' ;
	sharedPools: {};
	package: ''MyPackage'''.
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
		ensure: [ self removeFromSystem  ]
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testInstalledMinimalExceptOneMockClassRecreatorIsMinimal [

	| shiftClassBuilder installedClass |
	self removeFromSystem.
	[ 
	builder := self class compilerClass new
		           evaluate: 'Object < #MyClass
	layout: FixedLayout;
	uses: {};
	slots: { #x };
	sharedVariables: {};
	tag: '''' ;
	sharedPools: {};
	package: ''FluidClassDefinition'''.
	shiftClassBuilder := builder shiftClassBuilder.
	ShiftClassInstaller new makeWithBuilder: shiftClassBuilder.
	
	installedClass := self class environment at: #MyClass ifAbsent: [self fail].
	self assert: installedClass asFluidDefinitionString  equals: 'Object < #MyClass
	slots: { #x };
	package: ''FluidClassDefinition''']
		ensure: [ self removeFromSystem  ]
]

{ #category : #'tests - mandatory' }
FluidClassBuilderTest >> testInstalledMinimalMockClassRecreatorIsMinimal [

	| shiftClassBuilder installedClass |
	self removeFromSystem.
	[ 
	builder := self class compilerClass new
		           evaluate: 'Object < #MyClass
	layout: FixedLayout;
	uses: {};
	slots: {};
	sharedVariables: {};
	tag: '''' ;
	sharedPools: {};
	package: ''FluidClassDefinition'''.
	shiftClassBuilder := builder shiftClassBuilder.
	ShiftClassInstaller new makeWithBuilder: shiftClassBuilder.
	
	installedClass := self class environment at: #MyClass ifAbsent: [self fail].
	         
	self assert: installedClass asFluidDefinitionString equals: 'Object < #MyClass
	package: ''FluidClassDefinition''']
		ensure: [ self removeFromSystem  ]
]
