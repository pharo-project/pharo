"
I am testing the creation of Classes from an AST
"
Class {
	#name : #ASTClassBuilderTest,
	#superclass : #TestCase,
	#category : #'ClassParser-Tests'
}

{ #category : #environmentSetUp }
ASTClassBuilderTest >> environment [
	^ ShSmalltalkGlobalsEnvironment new.
]

{ #category : #running }
ASTClassBuilderTest >> parserClass [

	^ CDClassDefinitionParser
]

{ #category : #running }
ASTClassBuilderTest >> testCreateEphemeronClassNamed [

	| ast class |
	ast := self parserClass parse:
		       'Class ephemeronSubclass: #TestSubClass'.

	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isEphemeronClass.
	self assert: class name equals: #TestSubClass
]

{ #category : #running }
ASTClassBuilderTest >> testCreateEphemeronClassWithAll [

	| ast class |
	ast := self parserClass parse:
		       'Class ephemeronSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
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

{ #category : #running }
ASTClassBuilderTest >> testCreateImmediateClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class immediateSubclass: #TestSubClass'.

	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isImmediateClass.
	self assert: class name equals: #TestSubClass
]

{ #category : #running }
ASTClassBuilderTest >> testCreateImmediateClassWithAll [

	| ast class |
	ast := self parserClass parse:
		       'Class immediateSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
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

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassNamed [

	| ast class |
	ast := self parserClass parse: 'Object subclass: #TestClass'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class name equals: #TestClass
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassSuperClass [

	| ast class |
	ast := self parserClass parse: 'Xcrd subclass: #TestSubClass'.
	class := ShiftClassBuilder new
		         useUndefinedClass;
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self
		assert: class superclass
		equals: (self environment classNamed: 'Xcrd')
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassWithClassVariable [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															classVariableNames: ''var classVar pouet'' '.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassWithIV [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															instanceVariableNames: '''''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class instanceVariablesString equals: ''
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassWithPackage [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class category equals: #Unclassified
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassWithSharedPools [

	| ast class resTest |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															poolDictionaries: ''TestSharedPool ShTestSharedPool'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	resTest := OrderedCollection new.
	resTest add: 'TestSharedPool' asSymbol.
	resTest add: 'ShTestSharedPool' asSymbol.
	self assert: class sharedPoolNames equals: resTest
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassWithSlots [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															slots: {
																''cat'' => PropertySlot.
																''env'' => PropertySlot.
															}.'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class instanceVariablesString equals: 'cat env'
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassWithTrait [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															uses: TOccurrencesTest'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: (class isComposedBy: TOccurrencesTest)
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateNormalClassWithTraitComposition [

	| ast class |
	ast := self parserClass parse: 'Class subclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest + TCloneTest + TCopyTest + TRemoveTest + TGrowableTest + TStructuralEqualityTest + TPrintTest + TAsStringCommaAndDelimiterTest + TConvertTest + TConvertAsSortedTest + TConcatenationEqualElementsRemovedTest + TOccurrencesTest + (TCreationWithTest - {#testOfSize. #collectionClass}) + (TSetArithmetic - {#collectionClass})'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: (class isComposedBy: TAddForUniquenessTest).
	self assert: (class isComposedBy: TIncludesWithIdentityCheckTest)
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateTraitUsingAnotherSingleTrait [

	| ast class |
	ast := self parserClass parse: 'Trait named: #TTestTrait
															uses: TOccurrencesTest'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: (class isComposedBy: TOccurrencesTest)
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateTraitUsingAnotherTraitComposition [

	| ast class |
	ast := self parserClass parse: 'Trait named: #TTestTrait
															uses: TOccurrencesTest + TAddForUniquenessTest'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: (class isComposedBy: TOccurrencesTest).
	self assert: (class isComposedBy: TAddForUniquenessTest)
]

{ #category : #tests }
ASTClassBuilderTest >> testCreateTraitWithoutOtherTraits [

	| ast class |
	ast := self parserClass parse: 'Trait named: #TTestTrait
															uses: {}'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class traits isEmpty
]

{ #category : #running }
ASTClassBuilderTest >> testCreateVariableByteClassNamed [

	| ast class |
	ast := self parserClass parse:
		       'Object variableByteSubclass: #TestSubClass'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class isBytes.
	self assert: class name equals: #TestSubClass
]

{ #category : #running }
ASTClassBuilderTest >> testCreateVariableByteClassWithAll [

	| ast class |
	ast := self parserClass parse:
		       'Object variableByteSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : #running }
ASTClassBuilderTest >> testCreateVariableClassNamed [

	| ast class |
	ast := self parserClass parse:
		       'Class variableSubclass: #TestSubClass'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class isVariable.
	self assert: class name equals: #TestSubClass
]

{ #category : #running }
ASTClassBuilderTest >> testCreateVariableClassWithAll [

	| ast class |
	ast := self parserClass parse:
		       'Class variableSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
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

{ #category : #running }
ASTClassBuilderTest >> testCreateVariableWordClassNamed [

	| ast class |
	ast := self parserClass parse:
		       'Object variableWordSubclass: #TestSubClass'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class isWords.
	self assert: class name equals: #TestSubClass
]

{ #category : #running }
ASTClassBuilderTest >> testCreateVariableWordClassWithAll [

	| ast class |
	ast := self parserClass parse:
		       'Object variableWordSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #( #classVar #pouet #var )
]

{ #category : #running }
ASTClassBuilderTest >> testCreateWeakClassNamed [

	| ast class |
	ast := self parserClass parse: 'Class weakSubclass: #TestSubClass'.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isWeak.
	self assert: class name equals: #TestSubClass
]

{ #category : #running }
ASTClassBuilderTest >> testCreateWeakClassWithAll [

	| ast class |
	ast := self parserClass parse: 'Class weakSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		         buildEnvironment: self environment;
		         buildFromAST: ast;
		         build.

	self assert: class isWeak.
	self assert: class name equals: #TestSubClass.
	self assert: class instanceVariablesString equals: 'cat env'
]

{ #category : #resolveSuperclass }
ASTClassBuilderTest >> testDefaultResolve [

	| ast |
	ast := self parserClass parse: 'cdfgdf subclass: #TestSubClass
															uses: TOccurrencesTest'.
	self
		should: [ 
			ShiftClassBuilder new
				buildEnvironment: self environment;
				buildFromAST: ast;
				build ]
		raise: Error
]

{ #category : #resolveSuperclass }
ASTClassBuilderTest >> testUseUndefinedClass [

	| ast |
	ast := self parserClass parse: 'PoPouet subclass: #TestClass'.
	ShiftClassInstaller new make: [ :builder | 
		builder
			useUndefinedClass;
			buildEnvironment: self environment;
			buildFromAST: ast ].
	self assert: [ 
		(ShSmalltalkGlobalsEnvironment new classNamed: #PoPouet) isUndefined ]
]
