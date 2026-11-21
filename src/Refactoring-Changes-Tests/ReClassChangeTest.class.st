Class {
	#name : 'ReClassChangeTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'tests' }
ReClassChangeTest >> testIsInScopeKeepsChangesForClassesNotYetCreated [
	"Regression for #20239: duplicate/create class preview must not send isMeta to nil
	when the new class is not bound yet."

	| change env |
	change := ReAddClassChange make: [ :builder |
			builder
				superclass: Object;
				name: #ReClassChangeTestTemporaryClass;
				package: 'Refactoring-Changes-Tests' ].
	env := RBClassEnvironment classes: { Object }.

	self assert: change changeClass isNil.
	self assert: (change isInScope: env)
]
