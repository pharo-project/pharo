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
ASTClassBuilderTest >> setUp [ 

super setUp.
]

{ #category : #running }
ASTClassBuilderTest >> tearDown [
super tearDown.
]

{ #category : #running }
ASTClassBuilderTest >> testCreateEphemeronClassNamed [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class ephemeronSubclass: #TestSubClass'.
	
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

	ast := CDClassDefinitionParser parse: 'Class ephemeronSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes''' .
															
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: class isEphemeronClass.
	self assert:	class name equals: #TestSubClass	.
	self assert: class instanceVariablesString equals: 'cat env'.
	self assert: class classVarNames equals: #(#classVar #pouet #var)
]

{ #category : #running }
ASTClassBuilderTest >> testCreateImmediateClassNamed [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class immediateSubclass: #TestSubClass'.
	
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

	ast := CDClassDefinitionParser parse: 'Class immediateSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes''' .
															
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
	self assert: class isImmediateClass.
	self assert:	class name equals: #TestSubClass	.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #(#classVar #pouet #var)
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassNamed [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Object subclass: #TestClass'.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: class name equals: #TestClass
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassSuperClass [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Xcrd subclass: #TestSubClass'.
															
	
	class := ShiftClassBuilder new
	useUndefinedClass;
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: class superclass equals: (self environment classNamed: 'Xcrd').
	
	
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassWithClassVariable [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class subclass: #TestSubClass
															classVariableNames: ''var classVar pouet'' '.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: class classVarNames equals: #(#classVar #pouet #var)
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassWithIV [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class subclass: #TestSubClass
															instanceVariableNames: '''''.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: class instanceVariablesString equals: ''
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassWithPackage [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class subclass: #TestSubClass
															package: ''Kernel-Classes'''.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
			
	self assert: class category equals: #Unclassified

]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassWithSharedPools [

   | ast class resTest|

	ast := CDClassDefinitionParser parse: 'Class subclass: #TestSubClass
															poolDictionaries: ''TestSharedPool ShTestSharedPool'''.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
	resTest:= OrderedCollection new.
	resTest add: 'TestSharedPool' asSymbol.
	resTest add: 'ShTestSharedPool' asSymbol.
	self assert: class sharedPoolNames equals: resTest.
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassWithSlots [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class subclass: #TestSubClass
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

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassWithTrait [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class subclass: #TestSubClass
															uses: TOccurrencesTest'.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: (class isComposedBy: TOccurrencesTest).
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateNormalClassWithTraitComposition [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class subclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest + TCloneTest + TCopyTest + TRemoveTest + TGrowableTest + TStructuralEqualityTest + TPrintTest + TAsStringCommaAndDelimiterTest + TConvertTest + TConvertAsSortedTest + TConcatenationEqualElementsRemovedTest + TOccurrencesTest + (TCreationWithTest - {#testOfSize. #collectionClass}) + (TSetArithmetic - {#collectionClass})'.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: (class isComposedBy: TAddForUniquenessTest).
	self assert: (class isComposedBy: TIncludesWithIdentityCheckTest).
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateTraitUsingAnotherSingleTrait [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Trait named: #TTestTrait
															uses: TOccurrencesTest'.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
	
	self assert: (class isComposedBy: TOccurrencesTest).
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateTraitUsingAnotherTraitComposition [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Trait named: #TTestTrait
															uses: TOccurrencesTest + TAddForUniquenessTest'.
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
	
	self assert: (class isComposedBy: TOccurrencesTest).
	self assert: (class isComposedBy: TAddForUniquenessTest).
]

{ #category : #NormalClass }
ASTClassBuilderTest >> testCreateTraitWithoutOtherTraits [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Trait named: #TTestTrait
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

	ast := CDClassDefinitionParser parse: 'Object variableByteSubclass: #TestSubClass'.
	
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

	ast := CDClassDefinitionParser parse: 'Object variableByteSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes''' .
															
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert:	class name equals: #TestSubClass	.
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #(#classVar #pouet #var)
]

{ #category : #running }
ASTClassBuilderTest >> testCreateVariableClassNamed [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class variableSubclass: #TestSubClass'.
	
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

	ast := CDClassDefinitionParser parse: 'Class variableSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes''' .
															
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: class isVariable.
	self assert:	class name equals: #TestSubClass	.
	self assert: class instanceVariablesString equals: 'cat env'.
	self assert: class classVarNames equals: #(#classVar #pouet #var)
]

{ #category : #running }
ASTClassBuilderTest >> testCreateVariableWordClassNamed [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Object variableWordSubclass: #TestSubClass'.
	
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

	ast := CDClassDefinitionParser parse: 'Object variableWordSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes''' .
															
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert:	class name equals: #TestSubClass	.
			"should not have IV"
	self assert: class instanceVariablesString equals: ''.
	self assert: class classVarNames equals: #(#classVar #pouet #var)
]

{ #category : #running }
ASTClassBuilderTest >> testCreateWeakClassNamed [

   | ast class |

	ast := CDClassDefinitionParser parse: 'Class weakSubclass: #TestSubClass'.
	
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

	ast := CDClassDefinitionParser parse: 'Class weakSubclass: #TestSubClass
															uses:   TAddForUniquenessTest + TIncludesWithIdentityCheckTest
															instanceVariableNames: ''cat env''
															classVariableNames: ''var classVar pouet''
															poolDictionaries: ''TestSharedPool ShTestSharedPool''
															package: ''Kernel-Classes''' .
															
	class := ShiftClassBuilder new
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.
		
	self assert: class isWeak.
	self assert:	class name equals: #TestSubClass	.
	self assert: class instanceVariablesString equals: 'cat env'
]

{ #category : #resolveSuperclass }
ASTClassBuilderTest >> testDefaultResolve [
	| ast |

	ast := CDClassDefinitionParser parse: 'cdfgdf subclass: #TestSubClass
															uses: TOccurrencesTest'.
		
		self should:[ShiftClassBuilder new 
		buildEnvironment: self environment;
		buildFromAST: ast;
		build.] raise: Error.
]

{ #category : #resolveSuperclass }
ASTClassBuilderTest >> testUseUndefinedClass [
 	
   | ast |

	ast := CDClassDefinitionParser parse: 'PoPouet subclass: #TestClass'.

	ShiftClassInstaller new
		make: [ :builder | builder 
					useUndefinedClass;
					buildEnvironment: self environment;
					buildFromAST: ast
			 ].

self assert: [ (ShSmalltalkGlobalsEnvironment new classNamed: #PoPouet) isUndefined]
]
