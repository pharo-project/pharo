Class {
	#name : #TraitMethodDescriptionTest,
	#superclass : #TraitsTestCase,
	#category : 'Tests-Traits'
}

{ #category : #testing }
TraitMethodDescriptionTest >> testArgumentNames [
	self t1 compile: 'zork1: myArgument zork2: mySecondArgument ^true'.
	self t2 compile: 'zork1: myArgument zork2: somethingElse ^false'.
	self assert: ((self t5 sourceCodeAt: #zork1:zork2:) asString 
				beginsWith: 'zork1: arg1 zork2: arg2').
	self t1 compile: 'zork1: myArgument zork2: mySecondArgument ^true'.
	self t2 compile: 'zork1: somethingElse zork2: myArgument ^false'.
	self assert: ((self t5 sourceCodeAt: #zork1:zork2:) asString 
				beginsWith: 'zork1: arg1 zork2: arg2')
]

{ #category : #testing }
TraitMethodDescriptionTest >> testCategories [
	self assert: (self t4 organization categoryOfElement: #m21) equals: #cat1.
	self assert: (self t4 organization categoryOfElement: #m22) equals: #cat2.
	self assert: (self t4 organization categoryOfElement: #m11) equals: #catX.
	self assert: (self t4 organization categoryOfElement: #m12) equals: #cat2.
	self assert: (self t4 organization categoryOfElement: #m13) equals: #cat3.
	self assert: (self t6 organization categoryOfElement: #m22Alias) equals: #cat2.
	self t2 organization classify: #m22 under: #catX.
	self assert: (self t4 organization categoryOfElement: #m22) equals: #catX.
	self assert: (self t6 organization categoryOfElement: #m22Alias) equals: #catX.
	self t6 organization classify: #m22 under: #catY.
	self t6 organization classify: #m22Alias under: #catY.
	self t2 organization classify: #m22 under: #catZ.
	self assert: (self t6 organization categoryOfElement: #m22) equals: #catY.
	self assert: (self t6 organization categoryOfElement: #m22Alias) equals: #catY.
	self t1 compile: 'mA' classified: #catA.
	self assert: (self t4 organization categoryOfElement: #mA) equals: #catA.
	self t1 organization classify: #mA under: #cat1.
	self assert: (self t4 organization categories includes: #catA) not
]

{ #category : #testing }
TraitMethodDescriptionTest >> testConflictMethodCreation [
	"Generate conflicting methods between t1 and t2
	and check the resulting method in Trait t5 (or c2).
	Also test selectors like foo:x (without space) or selectors with CRs."

	"unary"

	self t2 compile: 'm12 ^false'.
	self assert: ((self t5 sourceCodeAt: #m12) asString beginsWith: 'm12').
	self should: [self c2 new m12] raise: Error.

	"binary"
	self t1 compile: '@ myArgument ^true'.
	self t2 compile: '@myArgument ^false'.
	self 
		assert: ((self t5 sourceCodeAt: #@) asString beginsWith: '@ anObject').
	self should: [self c2 new @ 17] raise: Error.

	"keyword"
	self t1 compile: 'zork: myArgument
		^true'.
	self t2 compile: 'zork: myArgument ^false'.
	self assert: ((self t5 sourceCodeAt: #zork:) asString 
				beginsWith: 'zork: arg1').
	self should: [self c2 new zork: 17] raise: Error.
	self t1 compile: 'zork:myArgument ^true'.
	self t2 compile: 'zork:myArgument ^false'.
	self assert: ((self t5 sourceCodeAt: #zork:) asString 
				beginsWith: 'zork: arg1').
	self should: [self c2 new zork: 17] raise: Error.
	self t1 compile: 'zork1: t1 zork2: t2 ^true'.
	self t2 compile: 'zork1: anObject zork2: anotherObject ^false'.
	self assert: ((self t5 sourceCodeAt: #zork1:zork2:) asString 
				beginsWith: 'zork1: arg1 zork2: arg2').
	self should: [self c2 new zork1: 1 zork2: 2] raise: Error
]

{ #category : #testing }
TraitMethodDescriptionTest >> testConflictingCategories [
	| t7 t8 |
	self t2 compile: 'm11' classified: #catY.
	self assert: (self t4 organization categoryOfElement: #m11) equals: #catX.
	self assert: (self t5 organization categoryOfElement: #m11) equals: #cat1.
	t7 := self createTraitNamed: #T7 uses: self t1 + self t2.
	self assert: (t7 organization categoryOfElement: #m11) 
				equals: Protocol ambiguous.
	self t1 removeSelector: #m11.
	self assert: (self t4 organization categoryOfElement: #m11) equals: #catX.
	self assert: (self t5 organization categoryOfElement: #m11) equals: #catY.
	self assert: (t7 organization categoryOfElement: #m11) equals: #catY.
	self assert: (t7 organization categories includes: Protocol ambiguous) not.
	self t1 compile: 'm11' classified: #cat1.
	t8 := self createTraitNamed: #T8 uses: self t1 + self t2.
	t8 organization classify: #m11 under: #cat1.
	self t1 organization classify: #m11 under: #catZ.
	self assert: (self t4 organization categoryOfElement: #m11) equals: #catX.
	self assert: (self t5 organization categoryOfElement: #m11) equals: #catY.
	self assert: (t8 organization categoryOfElement: #m11) equals: #catZ
]

{ #category : #testing }
TraitMethodDescriptionTest >> testInitialize [
	| empty |
	empty := TraitMethodDescription new.
	self assert: empty isEmpty.
	self deny: empty isConflict.
	self deny: empty isProvided.
	self deny: empty isRequired
]
