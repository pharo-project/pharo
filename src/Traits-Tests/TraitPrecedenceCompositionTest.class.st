Class {
	#name : 'TraitPrecedenceCompositionTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testClassCompositionOnPrecedenceKeepsPreference [

	| t1 t2 original copy |
	t1 := self newTrait: #T1.
	t1 classTrait uses: {  } slots: #(  ).

	t2 := self newTrait: #T2.
	t2 classTrait uses: {  } slots: #(  ).

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.

	original := t2 + t1 withPrecedenceOf: t2.
	copy := original classComposition.

	self assert: original preferedTrait innerClass equals: copy preferedTrait innerClass instanceSide
]

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testCopyingAPrecedenceKeepsPreference [

	| t1 t2 original copy |

	t1 := self newTrait: #T1.
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2.
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.

	original := (t2 + t1) withPrecedenceOf: t2.
	copy := original copyTraitExpression.

	self assert: original preferedTrait equals: copy preferedTrait
]

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testPrecedencesAreGeneratedInClassDefinition [

	| t1 t2 t3 c1 |
	t1 := self newTrait: #T1.
	t1 classTrait uses: {  } slots: #(  ).

	t2 := self newTrait: #T2.
	t2 classTrait uses: {  } slots: #(  ).

	t3 := self newTrait: #T3.
	t3 classTrait uses: {  } slots: #(  ).

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	t3 compile: 'm2 ^ 11'.

	c1 := self newClass: #C1 traits: (t2 + t1 + t3 withPrecedenceOf: t2).

	self assert: (ClassDefinitionPrinter oldPharo for: c1) definitionString equals: 'Object subclass: #C1
	uses: (T2 + T1 + T3 withPrecedenceOf: T2)
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''' , self packageNameForTests , ''''
]

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testPrecedencesAreGeneratedInClassDefinitionWithAlias [

	| t1 t2 t3 c1 |
	t1 := self newTrait: #T1.
	t1 classTrait uses: {  } slots: #(  ).

	t2 := self newTrait: #T2.
	t2 classTrait uses: {  } slots: #(  ).

	t3 := self newTrait: #T3.
	t3 classTrait uses: {  } slots: #(  ).

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	t3 compile: 'm2 ^ 11'.

	c1 := self newClass: #C1  traits: (t2 + (t1 -- #aSlot) + t3 withPrecedenceOf: t2).

	self assert: (ClassDefinitionPrinter oldPharo for: c1) definitionString equals: 'Object subclass: #C1
	uses: (T2 + (T1 -- #aSlot) + T3 withPrecedenceOf: T2)
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''' , self packageNameForTests , ''''
]

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testPrecedencesCanBeCombined [

	| t1 t2 t3 c1 |
	<ignoreNotImplementedSelectors: #(m1 m2)>
	t1 := self newTrait: #T1.
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2.
	t2 classTrait uses: {} slots: #().

	t3 := self newTrait: #T3.
	t3 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.

	t3 compile: 'm2 ^ 11'.

	c1 := self newClass: #C1 traits: (((t2 + t1) withPrecedenceOf: t2) + t3).

	self assert: (c1 new m1) equals: 33.
	self assert: (c1 new m2) equals: 11
]

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testWithPrecedenceIsNonAConflict [

	| t1 t2 c1 |

	t1 := self newTrait: #T1.
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2.
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.

	c1 := self newClass: #C1 traits: ((t1 + t2) withPrecedenceOf: t2).

	self deny: (c1 >> #m1) isConflict
]

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testWithPrecedenceUsesThePreferedOne [
	<ignoreNotImplementedSelectors: #(m1)>
	| t1 t2 c1 |

	t1 := self newTrait: #T1.
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2.
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.

	c1 := self newClass: #C1 traits: ((t1 + t2) withPrecedenceOf: t2).

	self assert: (c1 new m1) equals: 33
]

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testWithPrecedenceUsesThePreferedOneWithoutCaringOrderOfSequence [
	<ignoreNotImplementedSelectors: #(m1)>
	| t1 t2 c1 |

	t1 := self newTrait: #T1.
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2.
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.

	c1 := self newClass: #C1 traits: ((t2 + t1) withPrecedenceOf: t2).

	self assert: (c1 new m1) equals: 33
]

{ #category : 'tests' }
TraitPrecedenceCompositionTest >> testWithoutPrecedenceIsAConflict [

	| t1 t2 c1 |

	t1 := self newTrait: #T1.
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2.
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.

	c1 := self newClass: #C1 traits: t1 + t2.

	self assert: (c1 >> #m1) isConflict
]
