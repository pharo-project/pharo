Class {
	#name : #RBDeprecateMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBDeprecateMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBDeprecateMethodRefactoring };
		yourself
]

{ #category : #tests }
RBDeprecateMethodParametrizedTest >> testDeprecateMethodUsingMethodWithSameNumberOfArgs [

	| refactoring class oldSelector newSelector |
	oldSelector := #callMethod.
	newSelector := #callFoo.
	refactoring := rbClass 
							model: model 
							deprecateMethod: oldSelector 
							in: RBRefactoryTestDataApp 
							using: newSelector .
	
	self executeRefactoring: refactoring .
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert: ((class parseTreeFor: oldSelector ) statements anySatisfy: 
			[ :e | e isMessage ifTrue: [ e selector = #deprecated:on:in: ] ]).
]

{ #category : #tests }
RBDeprecateMethodParametrizedTest >> testDeprecateMethodUsingMethodWithoutArgs [

	| refactoring class oldSelector newSelector |
	oldSelector := ('called:', 'on:') asSymbol.
	newSelector := 'callFoo' asSymbol.
	refactoring := rbClass 
							model: model 
							deprecateMethod: oldSelector 
							in: RBRefactoryTestDataApp 
							using: newSelector .
	
	self executeRefactoring: refactoring .
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert: ((class parseTreeFor: oldSelector ) statements anySatisfy: 
			[ :e | e isMessage ifTrue: [ e selector = #deprecated:on:in: ] ]).
	
]

{ #category : #'failure tests' }
RBDeprecateMethodParametrizedTest >> testInvalidNewSelector [

	| refactoring oldSelector newSelector |
	oldSelector := ('called:', 'on1:') asSymbol.
	newSelector := 'inlineFoo1:' asSymbol.
	refactoring := rbClass 
							model: model 
							deprecateMethod: oldSelector 
							in: RBRefactoryTestDataApp 
							using: newSelector .
	
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBDeprecateMethodParametrizedTest >> testInvalidNumArgsOfSelector [

	| refactoring oldSelector newSelector |
	oldSelector := ('called:', 'on1:') asSymbol.
	newSelector := 'inlineFoo:' asSymbol.
	refactoring := rbClass 
							model: model 
							deprecateMethod: oldSelector 
							in: RBRefactoryTestDataApp 
							using: newSelector .
	
	self shouldFail: refactoring
]
