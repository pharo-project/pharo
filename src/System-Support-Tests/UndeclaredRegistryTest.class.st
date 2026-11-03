Class {
	#name : 'UndeclaredRegistryTest',
	#superclass : 'TestCase',
	#category : 'System-Support-Tests',
	#package : 'System-Support-Tests'
}

{ #category : 'tests' }
UndeclaredRegistryTest >> testVerifyEmptyRegistryDoesNotFail [
	"This is a smoke test"

	| registry |
	registry := UndeclaredRegistry new.
	self assert: registry isEmpty.
	registry verifyEmpty
]

{ #category : 'tests' }
UndeclaredRegistryTest >> testVerifyNonEmptyRegistryFails [
	"This is a smoke test"

	| registry |
	registry := UndeclaredRegistry new.
	registry add: (UndeclaredVariable key: #toto value: 17).

	self should: [registry verifyEmpty] raise: Error with: [ :err |
		self assert: err messageText equals: 'Undeclared registry is not empty. Offending entries: an Array(#toto->17->a WeakIdentitySet())'
	]
]
