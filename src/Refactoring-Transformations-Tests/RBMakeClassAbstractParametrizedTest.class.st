Class {
	#name : 'RBMakeClassAbstractParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#instVars : [
		'testClassName'
	],
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'building suites' }
RBMakeClassAbstractParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
<<<<<<< HEAD
		addCase: { 
			#rbClass -> RBMakeClassAbstractTransformation .
			#constructor -> #class: };
||||||| ec979f6c13
		addCase: { #rbClass -> RBMakeClassAbstractTransformation };
=======
		addCase: { 
			#rbClass -> RBMakeClassAbstractTransformation .
			#testClassName -> self unreferencedClassName .
			#constructor -> #classNamed: };
>>>>>>> add_subclass_refactoring_driver
		yourself
]

{ #category : 'running' }
RBMakeClassAbstractParametrizedTest >> setUp [
	super setUp.

<<<<<<< HEAD
	testClass := #[82 66 67 108 97 115 115 87 105 116 104 111 117 116 83 101 108 102 67 108 97 115 115 82 101 102 101 114 101 110 99 101 84 101 115 116] asString asSymbol.
	testClass class removeSelector: #isAbstract
||||||| ec979f6c13
	testClass := RBBasicLintRuleTestData.
	testClass class removeSelector: #isAbstract
=======
	testClassName := self unreferencedClassName.
	testClassName class removeSelector: #isAbstract
>>>>>>> add_subclass_refactoring_driver
]

{ #category : 'running' }
RBMakeClassAbstractParametrizedTest >> tearDown [

	(Smalltalk globals at: testClassName) class removeSelector: #isAbstract.
	super tearDown
]

{ #category : 'accessing' }
RBMakeClassAbstractParametrizedTest >> testClassName [

	^ testClassName
]

{ #category : 'accessing' }
RBMakeClassAbstractParametrizedTest >> testClassName: anObject [

	testClassName := anObject
]

{ #category : 'tests' }
RBMakeClassAbstractParametrizedTest >> testMakeClassAbstractAddsIsAbstractMethodToClassSide [
<<<<<<< HEAD
	| refactoring |
||||||| ec979f6c13
	| refactoring |
	refactoring := rbClass class: testClass.
=======
>>>>>>> add_subclass_refactoring_driver

<<<<<<< HEAD
	refactoring := self createRefactoringWithArguments: { testClass }.
||||||| ec979f6c13
=======
	| refactoring |
	refactoring := self createRefactoringWithArguments: { testClassName }.
>>>>>>> add_subclass_refactoring_driver
	self executeRefactoring: refactoring.
<<<<<<< HEAD
	self assert: ((refactoring model classNamed: testClass) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass)
||||||| ec979f6c13
	self assert: ((refactoring model classNamed: testClass name) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass name)
=======
	self 
		assert: ((refactoring model classNamed: testClassName) classSide parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClassName)
>>>>>>> add_subclass_refactoring_driver
]

{ #category : 'tests' }
RBMakeClassAbstractParametrizedTest >> testMakeClassAbstractPerformChanges [
	"This test checks if actually performing refactoring applies the changes
	since model needs to be set correctly. There was a regression when model
	was nil. This test case covers that. In future we should create tests
	that check `performChanges` logic."

	| refactoring |
<<<<<<< HEAD
	refactoring := self createRefactoringWithArguments: { testClass }.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: testClass) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass)
||||||| ec979f6c13
	refactoring := rbClass class: testClass.

	refactoring execute.
	self assert: ((refactoring model classNamed: testClass name) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass name)
=======
	refactoring := self createRefactoringWithArguments: { testClassName }.
	self executeRefactoring: refactoring.

	self 
		assert: ((refactoring model classNamed: testClassName) classSide parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClassName)
>>>>>>> add_subclass_refactoring_driver
]
