"
Unit test for categorization of traits
"
Class {
	#name : 'TraitWithMethodsInProtocolsTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitWithMethodsInProtocolsTest >> testPackageOfMethodFromTraits [

	| t1 t2 |
	t1 := self newTrait: #T1.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.

	t2 := self newTrait: #T2 traits: t1.

	self assert: (t1 >> #m1) protocolName equals: 'aProtocol'.
	self assert: (t2 >> #m1) protocolName equals: 'aProtocol'
]

{ #category : 'tests' }
TraitWithMethodsInProtocolsTest >> testPackageOfMethodFromTraitsAfterCreation [
	| t1 t2 |

	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2 traits: t1.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.

	self assert: (t1 >> #m1) protocolName equals: 'aProtocol'.
	self assert: (t2 >> #m1) protocolName equals: 'aProtocol'
]

{ #category : 'tests' }
TraitWithMethodsInProtocolsTest >> testPackageOfMethodFromTraitsChanged [
	| t1 t2 |

	t1 := self newTrait: #T1.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.

	t2 := self newTrait: #T2 traits: t1.

	t1 compile: 'm1 ^42.' classified: 'anotherProtocol'.

	self assert: (t1 >> #m1) protocolName equals: 'anotherProtocol'.
	self assert: (t2 >> #m1) protocolName equals: 'anotherProtocol'
]

{ #category : 'tests' }
TraitWithMethodsInProtocolsTest >> testPackageOfMethodFromTraitsChangedWithoutCompile [
	| trait1 trait2 |

	trait1 := self newTrait: #T1.
	trait1 compile: 'm1 ^42.' classified: 'aProtocol'.

	trait2 := self newTrait: #T2 traits: trait1.

	trait1 classify: #m1 under: 'anotherProtocol'.

	self assert: (trait1 >> #m1) protocolName equals: 'anotherProtocol'.
	self assert: (trait2 >> #m1) protocolName equals: 'anotherProtocol'
]

{ #category : 'tests' }
TraitWithMethodsInProtocolsTest >> testPackageOfMethodFromTraitsOverriden [
	| t1 t2 |

	t1 := self newTrait: #T1.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.

	t2 := self newTrait: #T2 traits: t1.
	t2 compile: 'm1 ^42.' classified: 'anotherProtocol'.

	self assert: (t1 >> #m1) protocolName equals: 'aProtocol'.
	self assert: (t2 >> #m1) protocolName equals: 'anotherProtocol'
]

{ #category : 'tests' }
TraitWithMethodsInProtocolsTest >> testPackageOfMethodFromTraitsRenamedCategory [

	| t1 t2 |
	t1 := self newTrait: #T1.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.
	t1 compile: 'm2 ^42.' classified: 'aProtocol'.

	t2 := self newTrait: #T2 traits: t1.

	t1 renameProtocol: 'aProtocol' as: 'anotherProtocol'.

	self assert: (t1 >> #m1) protocolName equals: 'anotherProtocol'.
	self assert: (t2 >> #m1) protocolName equals: 'anotherProtocol'.
	self assert: (t1 >> #m2) protocolName equals: 'anotherProtocol'.
	self assert: (t2 >> #m2) protocolName equals: 'anotherProtocol'
]
