Class {
	#name : 'ASTFluidClassBuilderTest',
	#superclass : 'TestCase',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'environmentSetUp' }
ASTFluidClassBuilderTest >> environment [

	^ self class environment
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> parserClass [

	^ CDFluidClassDefinitionParser
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateEphemeronClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass layout: EphemeronLayout'.

	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isEphemeronClass.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateEphemeronClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass
		layout: EphemeronLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.

	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isEphemeronClass.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateImmediateClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass layout: ImmediateLayout'.

	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isImmediateClass.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateImmediateClassWithAll [

	| ast class |
	ast := self parserClass parse:
		      'Class << #TestSubClass
		layout: ImmediateLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.

	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class isImmediateClass.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateNilSubclass [

	| ast class |
	ast := self parserClass parse: 'nil << #TestSubClass'.

	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class name equals: #TestSubClass.
	self assert: class superclass isNil
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateNormalClassNamed [

	| ast class |
	ast := self parserClass parse: 'Object << #TestClass'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class name equals: #TestClass
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateNormalClassSuperClass [

	| ast class |
	ast := self parserClass parse: 'Xcrd << #TestSubClass'.
	[
	class := ShiftClassBuilder new
		         useUndefinedClass;
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class superclass equals: (self environment classNamed: 'Xcrd') ] ensure: [ (self environment classNamed: 'Xcrd') ifNotNil: #removeFromSystem ]
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateNormalClassWithClassVariable [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass sharedVariables: { #var. #classVar. #pouet}'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateNormalClassWithIV [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass slots: { }'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class instanceVariablesString equals: ''
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateNormalClassWithPackage [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class category equals: Class unclassifiedCategory
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateNormalClassWithSharedPools [

	| ast class resTest |
	ast := self parserClass parse: 'Class << #TestSubClass sharedPools: #( #TestSharedPool #ShTestSharedPool )'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	resTest := OrderedCollection new.
	resTest add: 'TestSharedPool' asSymbol.
	resTest add: 'ShTestSharedPool' asSymbol.
	self assert: class sharedPoolNames equals: resTest
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateNormalClassWithTrait [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass traits: TOccurrencesTest'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: (class isComposedBy: TOccurrencesTest)
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateNormalClassWithTraitComposition [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest + TCloneTest + TCopyTest + TRemoveTest + TGrowableTest + TStructuralEqualityTest + TPrintTest + TAsStringCommaAndDelimiterTest + TConvertTest + TConvertAsSortedTest + TConcatenationEqualElementsRemovedTest + TOccurrencesTest + (TCreationWithTest - {#testOfSize. #collectionClass}) + (TSetArithmetic - {#collectionClass})'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: (class isComposedBy: TAddForUniquenessTest).
	self assert: (class isComposedBy: TIncludesWithIdentityCheckTest)
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateTraitUsingAnotherSingleTrait [

	| ast class |
	ast := self parserClass parse: 'Trait << #TTestTrait traits: TOccurrencesTest'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: (class isComposedBy: TOccurrencesTest)
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateTraitUsingAnotherTraitComposition [

	| ast class |
	ast := self parserClass parse: 'Trait << #TTestTrait traits: TOccurrencesTest + TAddForUniquenessTest'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: (class isComposedBy: TOccurrencesTest).
	self assert: (class isComposedBy: TAddForUniquenessTest)
]

{ #category : 'tests' }
ASTFluidClassBuilderTest >> testCreateTraitWithoutOtherTraits [

	| ast class |
	ast := self parserClass parse: 'Trait << #TTestTrait traits: { }'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class traits isEmpty
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateVariableByteClassNamed [

	| ast class |
	ast := self parserClass parse: 'Object << #TestSubClass layout: ByteLayout'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class isBytes.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateVariableByteClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Object << #TestSubClass
		layout: ByteLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateVariableClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass layout: VariableLayout'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class isVariable.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateVariableClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass
		layout: VariableLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class isVariable.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateVariableWordClassNamed [

	| ast class |
	ast := self parserClass parse: 'Object << #TestSubClass layout: WordLayout'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class isWords.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateVariableWordClassWithAll [

	| ast class |
	ast := self parserClass parse:
		       'Object << #TestSubClass
		layout: WordLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var ).
	self assert: class classLayout class equals: WordLayout
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateWeakClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass layout: WeakLayout'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isWeak.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTFluidClassBuilderTest >> testCreateWeakClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Class << #TestSubClass
		layout: WeakLayout;
		traits: TAddForUniquenessTest + TIncludesWithIdentityCheckTest;
		slots: { #cat. #env };
		sharedVariables: { #var. #classVar. #pouet };
		sharedPools: ''TestSharedPool ShTestSharedPool'';
		package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isWeak.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'
]

{ #category : 'resolveSuperclass' }
ASTFluidClassBuilderTest >> testDefaultResolve [

	| ast |
	ast := self parserClass parse: 'cdfgdf << #TestSubClass traits: TOccurrencesTest'.
	self
		should: [
			ShiftClassBuilder new
				buildEnvironment: self environment;
				buildFromAST: ast;
				build ]
		raise: Error
]

{ #category : 'resolveSuperclass' }
ASTFluidClassBuilderTest >> testUseUndefinedClass [

	| ast |
	ast := self parserClass parse: 'PoPouet << #TestClass'.
	[
	ShiftClassInstaller new make: [ :builder |
		builder
			useUndefinedClass;
			buildEnvironment: self environment;
			buildFromAST: ast ].
	self assert: [ (self environment at: #PoPouet) isUndefined ] ] ensure: [ self environment at: #PoPouet ifPresent: [ :class | class removeFromSystem ] ]
]
