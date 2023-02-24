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
	self assert: (c1 includesSelector: #m2)
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
	self assert: obj m2 equals: 2
]

{ #category : #tests }
T2TraitWithAliasTest >> testChangingDeleteSlot [

	| t1 c1 |

	t1 := self newTrait: #T1 with: #(ivar).
	t1 compile: 'ivar ^ ivar'.

	c1 := self newClass: #C1 with: #() uses: t1 -- {#ivar}.

	"we got the method #ivar from the Trait"
	self assert: (c1 includesSelector: #ivar).
	"the slot that we got from the trait is now removed"
	self assert: c1 slotNames equals: #().
	"make sure to not modify the Trait"
	self assert: t1 slotNames equals: #(ivar)
]

{ #category : #tests }
T2TraitWithAliasTest >> testChangingDeleteSlot2Slots [
	"we can add a slot in the class that we removed from the trait"
	| t1 c1 |

	t1 := self newTrait: #T1 with: #(ivar1 ivar2).
	t1 compile: 'ivar2 ^ ivar2'.
	t1 compile: 'ivar1 ^ ivar1'.

	c1 := self newClass: #C1 with: #() uses: t1 -- {  #ivar1 . #ivar2 }.

	"we got the methods from the Trait"
	self assert: (c1 includesSelector: #ivar1).
	self assert: (c1 includesSelector: #ivar2).

	"the slot that we got from the trait is now removed"
	self assert: c1 slotNames equals: #().
	"make sure to not modify the Trait"
	self assert: t1 slotNames equals: #(ivar1 ivar2)
]

{ #category : #tests }
T2TraitWithAliasTest >> testChangingDeleteSlotAddVar [

	| t1 c1 |

	t1 := self newTrait: #T1 with: #(ivar).
	t1 compile: 'ivar ^ ivar'.

	c1 := self newClass: #C1 with: #(ivar) uses: t1 -- {#ivar}.

	"we got the method #ivar from the Trait"
	self assert: (c1 includesSelector: #ivar).
	"the slot that we got from the trait is now removed"
	self assert: c1 slotNames equals: #(ivar).
	"make sure to not modify the Trait"
	self assert: t1 slotNames equals: #(ivar)
]

{ #category : #tests }
T2TraitWithAliasTest >> testChangingDeleteSlotAndAlias [
	"we can remove a slot from one trait, then rename from the other trait and the class has one var"
	| t1 t2 c1 |

	t1 := self newTrait: #T1 with: #(name).
	t2 := self newTrait: #T2 with: #(personName).

	c1 := self newClass: #C1 with: #() uses: (t1 -- #name) + (t2 @@ {#personName-> #name}).

	"In the class we have just name"
	self assert: c1 slotNames equals: #(name).
	"make sure to not modify the Trait"
	self assert: t1 slotNames equals: #(name).
	self assert: t2 slotNames equals: #(personName)
]

{ #category : #tests }
T2TraitWithAliasTest >> testChangingRenamedSlot [

	| t1 c1 |

	t1 := self newTrait: #T1 with: #(ivar).
	t1 compile: 'ivar ^ ivar'.

	c1 := self newClass: #C1 with: #() uses: t1 @@ { #ivar -> #ivar2 }.

	"we got the method #ivar from the Trait"
	self assert: (c1 includesSelector: #ivar).
	"the slot that we got from the trait is now called ivar2"
	self assert: c1 slotNames equals: #(ivar2).
	"was the code rewritten?"
	self assert: (c1>>#ivar) decompile printString equals: 'ivar

	^ ivar2'.

	"make sure to not modify the Trait"
	self assert: t1 slotNames equals: #(ivar)
]

{ #category : #tests }
T2TraitWithAliasTest >> testChangingRenamedSlot2Slots [

	| t1 c1 |

	t1 := self newTrait: #T1 with: #(ivar1 ivar2).
	t1 compile: 'ivar2 ^ ivar2'.
	t1 compile: 'ivar1 ^ ivar1'.

	c1 := self newClass: #C1 with: #() uses: t1 @@ {  #ivar1 -> #rvar1 . #ivar2 -> #rvar2 }.

	"we got the methods from the Trait"
	self assert: (c1 includesSelector: #ivar1).
	self assert: (c1 includesSelector: #ivar2).
	"but the code got rewritten to access the renamed variable"
	self assert: (c1>>#ivar1) decompile printString equals: 'ivar1

	^ rvar1'.

	self assert: (c1>>#ivar2) decompile printString equals: 'ivar2

	^ rvar2'
]
