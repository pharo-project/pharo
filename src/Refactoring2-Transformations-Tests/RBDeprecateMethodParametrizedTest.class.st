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
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
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
RBDeprecateMethodParametrizedTest >> testFailureInvalidNewSelector [
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."

	| refactoring oldSelector newSelector |
	oldSelector := ('called:' , 'on1:') asSymbol.
	newSelector := 'inlineFoo1:' asSymbol.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               oldSelector.
				               RBRefactoryTestDataApp.
				               newSelector }.

	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBDeprecateMethodParametrizedTest >> testFailureInvalidNumArgsOfSelector [
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."

	| refactoring oldSelector newSelector |
	oldSelector := ('called:' , 'on1:') asSymbol.
	newSelector := 'inlineFoo:' asSymbol.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               oldSelector.
				               RBRefactoryTestDataApp.
				               newSelector }.

	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBDeprecateMethodParametrizedTest >> testInvalidNewSelector [
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	
	| refactoring oldSelector newSelector |
	oldSelector := ('called:', 'on1:') asSymbol.
	newSelector := 'inlineFoo1:' asSymbol.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBRefactoryTestDataApp . newSelector }.

	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBDeprecateMethodParametrizedTest >> testInvalidNumArgsOfSelector [
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	
	| refactoring oldSelector newSelector |
	oldSelector := ('called:', 'on1:') asSymbol.
	newSelector := 'inlineFoo:' asSymbol.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ oldSelector . RBRefactoryTestDataApp . newSelector }.

	self shouldFail: refactoring
]
