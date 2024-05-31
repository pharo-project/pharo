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
		addCase: { 
			#rbClass -> RBMakeClassAbstractTransformation .
			#testClassName -> self unreferencedClassName .
			#constructor -> #classNamed: };
		yourself
]

{ #category : 'running' }
RBMakeClassAbstractParametrizedTest >> setUp [
	super setUp.

	testClassName := self unreferencedClassName.
	testClassName class removeSelector: #isAbstract
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

	| refactoring |
	refactoring := self createRefactoringWithArguments: { testClassName }.
	self executeRefactoring: refactoring.
	self 
		assert: ((refactoring model classNamed: testClassName) classSide parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClassName)
]

{ #category : 'tests' }
RBMakeClassAbstractParametrizedTest >> testMakeClassAbstractPerformChanges [
	"This test checks if actually performing refactoring applies the changes
	since model needs to be set correctly. There was a regression when model
	was nil. This test case covers that. In future we should create tests
	that check `performChanges` logic."

	| refactoring |
	refactoring := self createRefactoringWithArguments: { testClassName }.
	self executeRefactoring: refactoring.

	self 
		assert: ((refactoring model classNamed: testClassName) classSide parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClassName)
]
