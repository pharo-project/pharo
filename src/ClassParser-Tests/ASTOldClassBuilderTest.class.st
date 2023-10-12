"
I am testing the creation of Classes from an AST
"
Class {
	#name : 'ASTOldClassBuilderTest',
	#superclass : 'TestCase',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'running' }
ASTOldClassBuilderTest >> buildClassFrom: ast [

	^ ShiftClassBuilder new
		  buildEnvironment: self environment;
		  buildFromAST: ast;
		  build
]

{ #category : 'environmentSetUp' }
ASTOldClassBuilderTest >> environment [

	^ self class environment
]

{ #category : 'running' }
ASTOldClassBuilderTest >> parserClass [

	^ CDClassDefinitionParser
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateEphemeronClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class ephemeronSubclass: #TestSubClass'.

	class := self buildClassFrom: ast.

	self assert: class isEphemeronClass.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateEphemeronClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Class ephemeronSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.

	class := self buildClassFrom: ast.

	self assert: class isEphemeronClass.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateImmediateClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class immediateSubclass: #TestSubClass'.

	class := self buildClassFrom: ast.

	self assert: class isImmediateClass.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateImmediateClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Class immediateSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.

	class := self buildClassFrom: ast.
	self assert: class isImmediateClass.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateNormalClassNamed [

	| ast class |
	ast := self parserClass parse: 'Object subclass: #TestClass'.
	class := self buildClassFrom: ast.
	self assert: class name equals: #TestClass
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateNormalClassSuperClass [

	| ast class |
	ast := self parserClass parse: 'Xcrd subclass: #TestSubClass'.
	[
	class := ShiftClassBuilder new
		         useUndefinedClass;
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class superclass equals: (self environment classNamed: 'Xcrd') ] ensure: [
		self environment at: 'Xcrd' ifPresent: [ :aClass | aClass removeFromSystem ] ]
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateNormalClassWithClassVariable [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															classVariableNames: ''var classVar pouet'' '.
	class := self buildClassFrom: ast.

	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateNormalClassWithIV [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															instanceVariableNames: '''''.
	class := self buildClassFrom: ast.
	self assert: class instanceVariablesString equals: ''
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateNormalClassWithPackage [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															package: ''Kernel-Classes'''.
	class := self buildClassFrom: ast.
	self assert: class category equals: Class unclassifiedCategory
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateNormalClassWithSharedPools [

	| ast class resTest |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															poolDictionaries: ''TestSharedPool ShTestSharedPool'''.
	class := self buildClassFrom: ast.
	resTest := OrderedCollection new.
	resTest add: 'TestSharedPool' asSymbol.
	resTest add: 'ShTestSharedPool' asSymbol.
	self assert: class sharedPoolNames equals: resTest
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateNormalClassWithTrait [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															uses: TOccurrencesTest'.
	class := self buildClassFrom: ast.
	self assert: (class isComposedBy: TOccurrencesTest)
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateNormalClassWithTraitComposition [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest + TCloneTest + TCopyTest + TRemoveTest + TGrowableTest + TStructuralEqualityTest + TPrintTest + TAsStringCommaAndDelimiterTest + TConvertTest + TConvertAsSortedTest + TConcatenationEqualElementsRemovedTest + TOccurrencesTest + (TCreationWithTest - {#testOfSize. #collectionClass}) + (TSetArithmetic - {#collectionClass})'.
	class := self buildClassFrom: ast.
	self assert: (class isComposedBy: TAddForUniquenessTest).
	self assert: (class isComposedBy: TIncludesWithIdentityCheckTest)
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateTraitUsingAnotherSingleTrait [

	| ast class |
	ast := self parserClass parse: 'Trait named: #TTestTrait
															uses: TOccurrencesTest'.
	class := self buildClassFrom: ast.
	self assert: (class isComposedBy: TOccurrencesTest)
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateTraitUsingAnotherTraitComposition [

	| ast class |
	ast := self parserClass parse: 'Trait named: #TTestTrait
															uses: TOccurrencesTest + TAddForUniquenessTest'.
	class := self buildClassFrom: ast.
	self assert: (class isComposedBy: TOccurrencesTest).
	self assert: (class isComposedBy: TAddForUniquenessTest)
]

{ #category : 'tests' }
ASTOldClassBuilderTest >> testCreateTraitWithoutOtherTraits [

	| ast class |
	ast := self parserClass parse: 'Trait named: #TTestTrait
															uses: {}'.
	class := self buildClassFrom: ast.
	self assert: class traits isEmpty
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateVariableByteClassNamed [

	| ast class |
	ast := self parserClass parse: 'Object variableByteSubclass: #TestSubClass'.
	class := self buildClassFrom: ast.
	self assert: class isBytes.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateVariableByteClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Object variableByteSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.
	class := self buildClassFrom: ast.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateVariableClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class variableSubclass: #TestSubClass'.
	class := self buildClassFrom: ast.
	self assert: class isVariable.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateVariableClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Class variableSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.
	class := self buildClassFrom: ast.
	self assert: class isVariable.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateVariableWordClassNamed [

	| ast class |
	ast := self parserClass parse: 'Object variableWordSubclass: #TestSubClass'.
	class := self buildClassFrom: ast.
	self assert: class isWords.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateVariableWordClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Object variableWordSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.
	class := self buildClassFrom: ast.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var ).
	self assert: class classLayout class equals: WordLayout
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateWeakClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class weakSubclass: #TestSubClass'.
	class := self buildClassFrom: ast.

	self assert: class isWeak.
	self assert: class name equals: #TestSubClass
]

{ #category : 'running' }
ASTOldClassBuilderTest >> testCreateWeakClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Class weakSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.
	class := self buildClassFrom: ast.

	self assert: class isWeak.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'
]

{ #category : 'resolveSuperclass' }
ASTOldClassBuilderTest >> testDefaultResolve [

	| ast |
	ast := self parserClass parse: 'cdfgdf subclass: #TestSubClass
															uses: TOccurrencesTest'.
	self should: [ self buildClassFrom: ast ] raise: Error
]

{ #category : 'resolveSuperclass' }
ASTOldClassBuilderTest >> testUseUndefinedClass [

	| ast |
	ast := self parserClass parse: 'PoPouet subclass: #TestClass'.
	[
	ShiftClassInstaller new make: [ :builder |
		builder
			useUndefinedClass;
			buildEnvironment: self environment;
			buildFromAST: ast ].
	self assert: [ (self environment at: #PoPouet) isUndefined ] ] ensure: [ (self environment at: #PoPouet) ifNotNil: #removeFromSystem ]
]
