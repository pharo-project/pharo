Class {
	#name : 'RBMakeClassAbstractParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#instVars : [
		'testClass'
	],
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'building suites' }
RBMakeClassAbstractParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { 
			#rbClass -> RBMakeClassAbstractTransformation .
			#constructor -> #class: };
		yourself
]

{ #category : 'running' }
RBMakeClassAbstractParametrizedTest >> setUp [
	super setUp.

	testClass := #[82 66 67 108 97 115 115 87 105 116 104 111 117 116 83 101 108 102 67 108 97 115 115 82 101 102 101 114 101 110 99 101 84 101 115 116] asString asSymbol.
	testClass class removeSelector: #isAbstract
]

{ #category : 'running' }
RBMakeClassAbstractParametrizedTest >> tearDown [

	testClass class removeSelector: #isAbstract.
	super tearDown
]

{ #category : 'tests' }
RBMakeClassAbstractParametrizedTest >> testMakeClassAbstractAddsIsAbstractMethodToClassSide [
	| refactoring |

	refactoring := self createRefactoringWithArguments: { testClass }.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: testClass) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass)
]

{ #category : 'tests' }
RBMakeClassAbstractParametrizedTest >> testMakeClassAbstractPerformChanges [
	"This test checks if actually performing refactoring applies the changes
	since model needs to be set correctly. There was a regression when model
	was nil. This test case covers that. In future we should create tests
	that check `performChanges` logic."

	| refactoring |
	refactoring := self createRefactoringWithArguments: { testClass }.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: testClass) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass)
]
