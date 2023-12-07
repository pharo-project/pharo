Class {
	#name : 'TraitMethodDescriptionTest',
	#superclass : 'AbstractTraitsOnPreparedModelTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitMethodDescriptionTest >> testArgumentNames [

	self t1 compile: 'zork1: myArgument zork2: mySecondArgument ^true'.
	self t2 compile: 'zork1: myArgument zork2: somethingElse ^false'.
	self assert: ((self t5 sourceCodeAt: #zork1:zork2:) asString beginsWith: 'zork1: myArgument zork2: somethingElse').
	self t1 compile: 'zork1: myArgument zork2: mySecondArgument ^true'.
	self t2 compile: 'zork1: somethingElse zork2: myArgument ^false'.
	self assert: ((self t5 sourceCodeAt: #zork1:zork2:) asString beginsWith: 'zork1: somethingElse zork2: myArgument')
]

{ #category : 'tests' }
TraitMethodDescriptionTest >> testConflictMethodCreation [
	"Generate conflicting methods between t1 and t2
	and check the resulting method in Trait t5 (or c2).
	Also test selectors like foo:x (without space) or selectors with CRs."
		<ignoreNotImplementedSelectors: #(zork: m12 zork1:zork2: )>
	"unary"

	self t2 compile: 'm12 ^false'.
	self assert: ((self t5 sourceCodeAt: #m12) asString beginsWith: 'm12').
	self should: [self c2 new m12] raise: Error.

	"binary"
	self t1 compile: '@ myArgument ^true'.
	self t2 compile: '@myArgument ^false'.
	self
		assert: ((self t5 sourceCodeAt: #@) asString beginsWith: '@myArgument').
	self should: [self c2 new @ 17] raise: Error.

	"keyword"
	self t1 compile: 'zork: myArgument
		^true'.
	self t2 compile: 'zork: myArgument ^false'.
	self assert: ((self t5 sourceCodeAt: #zork:) asString
				beginsWith: 'zork: myArgument').
	self should: [self c2 new zork: 17] raise: Error.
	self t1 compile: 'zork:myArgument ^true'.
	self t2 compile: 'zork:myArgument ^false'.
	self assert: ((self t5 sourceCodeAt: #zork:) asString
				beginsWith: 'zork:myArgument').
	self should: [self c2 new zork: 17] raise: Error.
	self t1 compile: 'zork1: t1 zork2: t2 ^true'.
	self t2 compile: 'zork1: anObject zork2: anotherObject ^false'.
	self assert: ((self t5 sourceCodeAt: #zork1:zork2:) asString
				beginsWith: 'zork1: anObject zork2: anotherObject').
	self should: [self c2 new zork1: 1 zork2: 2] raise: Error
]

{ #category : 'tests' }
TraitMethodDescriptionTest >> testConflictingProtocols [

	| t7 tr8 |
	self t2 compile: 'm11' classified: #catY.
	self assert: (self t4 protocolOfSelector: #m11) name equals: #catX.
	self assert: (self t5 protocolOfSelector: #m11) name equals: Protocol traitConflictName.
	t7 := self newTrait: #T7 traits: self t1 + self t2.
	self assert: (t7 protocolOfSelector: #m11) name equals: Protocol traitConflictName.
	self t1 removeSelector: #m11.
	self assert: (self t4 protocolOfSelector: #m11) name equals: #catX.
	self assert: (self t5 protocolOfSelector: #m11) name equals: #catY.
	self assert: (t7 protocolOfSelector: #m11) name equals: #catY.
	self deny: (t7 hasProtocol: Protocol traitConflictName).
	self t1 compile: 'm11' classified: #cat1.
	tr8 := self newTrait: #T8 traits: self t1 + self t2.
	tr8 classify: #m11 under: #cat1.
	self t1 classify: #m11 under: #catZ.
	self assert: (self t4 protocolOfSelector: #m11) name equals: #catX.
	self assert: (self t5 protocolOfSelector: #m11) name equals: Protocol traitConflictName.
	self assert: (tr8 protocolOfSelector: #m11) name equals: #catZ
]

{ #category : 'tests' }
TraitMethodDescriptionTest >> testProtocols [

	self assert: (self t4 protocolOfSelector: #m21) name equals: #cat1.
	self assert: (self t4 protocolOfSelector: #m22) name equals: #cat2.
	self assert: (self t4 protocolOfSelector: #m11) name equals: #catX.
	self assert: (self t4 protocolOfSelector: #m12) name equals: #cat2.
	self assert: (self t4 protocolOfSelector: #m13) name equals: #cat3.
	self assert: (self t6 protocolOfSelector: #m22Alias) name equals: #cat2.
	self t2 classify: #m22 under: #catX.
	self assert: (self t4 protocolOfSelector: #m22) name equals: #catX.
	self assert: (self t6 protocolOfSelector: #m22Alias) name equals: #catX.
	self t6 classify: #m22 under: #catY.
	self t6 classify: #m22Alias under: #catY.
	self t2 classify: #m22 under: #catZ.
	self assert: (self t6 protocolOfSelector: #m22) name equals: #catY.
	self assert: (self t6 protocolOfSelector: #m22Alias) name equals: #catY.
	self t1 compile: 'mA' classified: #catA.
	self assert: (self t4 protocolOfSelector: #mA) name equals: #catA.
	self t1 classify: #mA under: #cat1.
	self assert: (self t4 hasProtocol: #catA) not
]
