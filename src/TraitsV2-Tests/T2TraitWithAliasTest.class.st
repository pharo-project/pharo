Class {
	#name : #T2TraitWithAliasTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitWithAliasTest >> testChangingAnAliasedMethodIsIncludedInClass [

	| t1 c1 |
	
	t1 := self newTrait: #T1 with: #().
	t1 compile: 'm1 ^ 1'.
	
	c1 := self newClass: #C1 with: #() uses: t1 @ { #m2 -> #m1 }.

	self assert: (c1 includesSelector: #m1).	
	self assert: (c1 includesSelector: #m2).
]

{ #category : #tests }
T2TraitWithAliasTest >> testChangingAnAliasedMethodIsUpdated [

	| t1 c1 obj |
	
	t1 := self newTrait: #T1 with: #().
	t1 compile: 'm1 ^ 1'.
	
	c1 := self newClass: #C1 with: #() uses: t1 @ { #m2 -> #m1 }.
	obj := c1 new.

	self assert: obj m1 equals: 1.
	self assert: obj m2 equals: 1.
	
	t1 compile: 'm1 ^ 2'.
	
	self assert: obj m1 equals: 2.
	self assert: obj m2 equals: 2.

]
