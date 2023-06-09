Class {
	#name : #RBDeprecateMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBDeprecateMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBDeprecateMethodRefactoring };
		addCase: { #rbClass -> RBDeprecateMethodTransformation };
		yourself
]

{ #category : #accessing }
RBDeprecateMethodParametrizedTest >> constructor [
	^ #deprecateMethod:in:using:
]

{ #category : #tests }
RBDeprecateMethodParametrizedTest >> testDeprecateMethodUsingMethodWithSameNumberOfArgs [

	| refactoring class oldSelector newSelector |

	oldSelector := #callMethod.
	newSelector := #callFoo.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBRefactoryTestDataApp . newSelector }.

	self executeRefactoring: refactoring .
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert: ((class parseTreeForSelector: oldSelector ) statements anySatisfy:
			[ :e | e isMessage ifTrue: [ e selector = #deprecated:on:in: ] ])
]

{ #category : #tests }
RBDeprecateMethodParametrizedTest >> testDeprecateMethodUsingMethodWithoutArgs [

	| refactoring class oldSelector newSelector |

	oldSelector := ('called:', 'on:') asSymbol.
	newSelector := 'callFoo' asSymbol.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBRefactoryTestDataApp . newSelector }.

	self executeRefactoring: refactoring .
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert: ((class parseTreeForSelector: oldSelector ) statements anySatisfy:
			[ :e | e isMessage and: [ e selector = #deprecated:on:in: ] ])
]

{ #category : #'failure tests' }
RBDeprecateMethodParametrizedTest >> testInvalidNewSelector [

	| refactoring oldSelector newSelector |
	oldSelector := ('called:', 'on1:') asSymbol.
	newSelector := 'inlineFoo1:' asSymbol.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBRefactoryTestDataApp . newSelector }.

	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBDeprecateMethodParametrizedTest >> testInvalidNumArgsOfSelector [

	| refactoring oldSelector newSelector |
	oldSelector := ('called:', 'on1:') asSymbol.
	newSelector := 'inlineFoo:' asSymbol.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBRefactoryTestDataApp . newSelector }.

	self shouldFail: refactoring
]
