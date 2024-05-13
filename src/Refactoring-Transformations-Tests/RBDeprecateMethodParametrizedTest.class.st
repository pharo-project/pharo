Class {
	#name : 'RBDeprecateMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'building suites' }
RBDeprecateMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBDeprecateMethodTransformation };
		yourself
]

{ #category : 'accessing' }
RBDeprecateMethodParametrizedTest >> constructor [
	^ #deprecateMethod:in:using:
]

{ #category : 'tests' }
RBDeprecateMethodParametrizedTest >> testDeprecateMethodUsingMethodWithSameNumberOfArgs [

	| refactoring class oldSelector newSelector |
	oldSelector := #callMethod.
	newSelector := #callFoo.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               oldSelector.
				               RBClassDataForRefactoringTest.
				               newSelector }.

	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self assert:
		((class parseTreeForSelector: oldSelector) statements anySatisfy: [
			 :e | e isMessage ifTrue: [ e selector = #deprecated:on:in: ] ]).
	
]

{ #category : 'tests' }
RBDeprecateMethodParametrizedTest >> testDeprecateMethodUsingMethodWithoutArgs [

	| refactoring class oldSelector newSelector |

	oldSelector := ('called:', 'on:') asSymbol.
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	newSelector := 'callFoo' asSymbol.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBClassDataForRefactoringTest . newSelector }.

	self executeRefactoring: refactoring .
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self
		assert: ((class parseTreeForSelector: oldSelector ) statements anySatisfy:
			[ :e | e isMessage and: [ e selector = #deprecated:on:in: ] ])
]

{ #category : 'tests' }
RBDeprecateMethodParametrizedTest >> testValidateBogusNewSelector [

	| refactoring oldSelector newSelector |

	oldSelector := #callMethod.
	newSelector := #'call_ crap Method'.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBClassDataForRefactoringTest . newSelector }.

	self should: [ refactoring checkPreconditions ]
		raise: RBRefactoringError, RBRefactoringWarning
]

{ #category : 'tests' }
RBDeprecateMethodParametrizedTest >> testValidateEmptyNewSelector [

	| refactoring oldSelector newSelector |

	oldSelector := #callMethod.
	newSelector := ''.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBClassDataForRefactoringTest . newSelector }.

	self should: [ refactoring checkPreconditions ]
		raise: RBRefactoringError, RBRefactoringWarning
]

{ #category : 'tests' }
RBDeprecateMethodParametrizedTest >> testValidateNewSelector [

	| refactoring oldSelector newSelector |

	oldSelector := #callMethod.
	newSelector := #callFoo.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBClassDataForRefactoringTest . newSelector }.

	self shouldnt: [ refactoring checkPreconditions ]
		raise: RBRefactoringError, RBRefactoringWarning
]

{ #category : 'tests' }
RBDeprecateMethodParametrizedTest >> testValidateSameNewSelector [

	| refactoring oldSelector newSelector |
	oldSelector := #callMethod.
	newSelector := #callMethod.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               oldSelector.
				               RBClassDataForRefactoringTest.
				               newSelector }.

	self
		should: [ refactoring checkPreconditions ]
		raise: RBRefactoringError
]
