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

	self
		should: [ registry verifyEmpty ]
		raise: Error
		with: [ :err | self assert: err messageText equals: 'Undeclared registry is not empty. Offending entries: {#toto->#()}' ]
]

{ #category : 'tests' }
UndeclaredRegistryTest >> testVerifyNonEmptyRegistryNamesTheMethodsUsingAnEntry [

	| registry |
	registry := UndeclaredRegistry new.
	registry add: (self class environment associationAt: #UndeclaredRegistry).

	self should: [ registry verifyEmpty ] raise: Error with: [ :err |
			self assert:
				(err messageText includesSubstring: (self class >> #testVerifyNonEmptyRegistryNamesTheMethodsUsingAnEntry) printString) ]
]

{ #category : 'tests' }
UndeclaredRegistryTest >> testVerifyNonEmptyRegistryOfAssociationsFails [
	"The bootstrap fills the registry with plain associations, not UndeclaredVariables."

	| registry |
	registry := UndeclaredRegistry new.
	registry add: #toto -> 17.

	self
		should: [ registry verifyEmpty ]
		raise: Error
		with: [ :err | self assert: err messageText equals: 'Undeclared registry is not empty. Offending entries: {#toto->#()}' ]
]
