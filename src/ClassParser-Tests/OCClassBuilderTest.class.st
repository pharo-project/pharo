Class {
	#name : 'OCClassBuilderTest',
	#superclass : 'TestCase',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'running' }
OCClassBuilderTest >> buildClassFromDefinition: aString [

	^ self buildClassFromDefinition: aString allowUndefined: false
]

{ #category : 'running' }
OCClassBuilderTest >> buildClassFromDefinition: aString allowUndefined: aBoolean [

	| ast builder |
	ast := CDFluidClassDefinitionParser parse: aString.

	builder := ShiftClassBuilder new buildEnvironment: self class environment.

	aBoolean ifTrue: [ builder useUndefinedClass ].

	^ builder
		  buildFromAST: ast;
		  build
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateEphemeronClassNamed [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass layout: EphemeronLayout'.

	self assert: class isEphemeronClass.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateEphemeronClassWithAll [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass
		layout: EphemeronLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.

	self assert: class isEphemeronClass.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateImmediateClassNamed [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass layout: ImmediateLayout'.

	self assert: class isImmediateClass.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateImmediateClassWithAll [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass
		layout: ImmediateLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.

	self assert: class isImmediateClass.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateNilSubclass [

	| class |
	class := self buildClassFromDefinition: 'nil << #TestSubClass'.

	self assert: class name equals: #TestSubClass.
	self assert: class superclass isNil
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateNormalClassNamed [

	| class |
	class := self buildClassFromDefinition: 'Object << #TestClass'.

	self assert: class name equals: #TestClass
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateNormalClassSuperClass [

	[
	| class |
	class := self buildClassFromDefinition: 'Xcrd << #TestSubClass' allowUndefined: true.

	self assert: class superclass equals: (self class environment classNamed: #Xcrd) ] ensure: [
		self class environment at: #Xcrd ifPresent: [ :aClass | aClass removeFromSystem ] ]
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateNormalClassWithClassVariable [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass sharedVariables: { #var. #classVar. #pouet}'.

	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateNormalClassWithIV [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass slots: { }'.

	self assert: class instanceVariablesString equals: ''
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateNormalClassWithPackage [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass package: ''Kernel-Classes'''.

	self assert: class package isUndefined
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateNormalClassWithSharedPools [

	| class resTest |
	class := self buildClassFromDefinition: 'Class << #TestSubClass sharedPools: { TestSharedPool . ShTestSharedPool }'.

	resTest := OrderedCollection new.
	resTest add: 'TestSharedPool' asSymbol.
	resTest add: 'ShTestSharedPool' asSymbol.
	self assert: class sharedPoolNames equals: resTest
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateNormalClassWithTrait [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass traits: TOccurrencesTest'.
	self assert: (class isComposedBy: TOccurrencesTest)
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateNormalClassWithTraitComposition [

	| class |
	class := self buildClassFromDefinition:
		         'Class << #TestSubClass traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest + TCloneTest + TCopyTest + TRemoveTest + TGrowableTest + TStructuralEqualityTest + TPrintTest + TAsStringCommaAndDelimiterTest + TConvertTest + TConvertAsSortedTest + TConcatenationEqualElementsRemovedTest + TOccurrencesTest + (TCreationWithTest - {#testOfSize. #collectionClass}) + (TSetArithmetic - {#collectionClass})'.

	self assert: (class isComposedBy: TAddForUniquenessTest).
	self assert: (class isComposedBy: TIncludesWithIdentityCheckTest)
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateTraitUsingAnotherSingleTrait [

	| class |
	class := self buildClassFromDefinition: 'Trait << #TTestTrait traits: TOccurrencesTest'.
	self assert: (class isComposedBy: TOccurrencesTest)
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateTraitUsingAnotherTraitComposition [

	| class |
	class := self buildClassFromDefinition: 'Trait << #TTestTrait traits: TOccurrencesTest + TAddForUniquenessTest'.
	self assert: (class isComposedBy: TOccurrencesTest).
	self assert: (class isComposedBy: TAddForUniquenessTest)
]

{ #category : 'tests' }
OCClassBuilderTest >> testCreateTraitWithoutOtherTraits [

	| class |
	class := self buildClassFromDefinition: 'Trait << #TTestTrait traits: { }'.
	self assertEmpty: class traits
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateVariableByteClassNamed [

	| class |
	class := self buildClassFromDefinition: 'Object << #TestSubClass layout: ByteLayout'.
	self assert: class isBytes.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateVariableByteClassWithAll [

	| class |
	class := self buildClassFromDefinition: 'Object << #TestSubClass
		layout: ByteLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.

	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateVariableClassNamed [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass layout: VariableLayout'.
	self assert: class isVariable.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateVariableClassWithAll [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass
		layout: VariableLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.

	self assert: class isVariable.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateVariableWordClassNamed [

	| class |
	class := self buildClassFromDefinition: 'Object << #TestSubClass layout: WordLayout'.

	self assert: class isWords.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateVariableWordClassWithAll [

	| class |
	class := self buildClassFromDefinition: 'Object << #TestSubClass
		layout: WordLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.

	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var ).
	self assert: class classLayout class equals: WordLayout
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateWeakClassNamed [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass layout: WeakLayout'.

	self assert: class isWeak.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
OCClassBuilderTest >> testCreateWeakClassWithAll [

	| class |
	class := self buildClassFromDefinition: 'Class << #TestSubClass
		layout: WeakLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.

	self assert: class isWeak.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'
]

{ #category : 'resolveSuperclass' }
OCClassBuilderTest >> testDefaultResolve [

	self should: [ self buildClassFromDefinition: 'cdfgdf << #TestSubClass traits: TOccurrencesTest' ] raise: Error
]

{ #category : 'resolveSuperclass' }
OCClassBuilderTest >> testUseUndefinedClass [

	| ast |
	ast := CDFluidClassDefinitionParser parse: 'PoPouet << #TestClass'.
	[
	ShiftClassInstaller new make: [ :builder |
		builder
			useUndefinedClass;
			buildEnvironment: self class environment;
			buildFromAST: ast ].
	self assert: [ (self class environment at: #PoPouet) isUndefined ] ] ensure: [
		self class environment at: #PoPouet ifPresent: [ :class | class removeFromSystem ] ]
]
