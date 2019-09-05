Class {
	#name : #T2PrecedenceCompositionTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests-Tests'
}

{ #category : #tests }
T2PrecedenceCompositionTest >> testClassCompositionOnPrecedenceKeepsPreference [

	| t1 t2 original copy |

	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	
	original := (t2 + t1) withPrecedenceOf: t2.
	copy := original classComposition.
	
	self 
		assert: original preferedTrait innerClass 
		equals: copy preferedTrait innerClass instanceSide.

]

{ #category : #tests }
T2PrecedenceCompositionTest >> testCopyingAPrecedenceKeepsPreference [

	| t1 t2 original copy |

	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	
	original := (t2 + t1) withPrecedenceOf: t2.
	copy := original copyTraitExpression.
	
	self assert: original preferedTrait equals: copy preferedTrait

]

{ #category : #tests }
T2PrecedenceCompositionTest >> testPrecedencesAreGeneratedInClassDefinition [
	| t1 t2 t3 c1 |
	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t3 := self newTrait: #T3 with: #() uses: #().
	t3 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	t3 compile: 'm2 ^ 11'.

	c1 := self newClass: #C1 with: #() uses: (t2 + t1 + t3 withPrecedenceOf: t2).

	self
		assert: c1 definition
		equals:
			(Slot showSlotClassDefinition
				ifFalse: [ 'Object subclass: #C1
	uses: (T2 + T1 + T3 withPrecedenceOf: T2)
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''TraitsV2-Tests-TestClasses''' ]
				ifTrue: [ 'Object subclass: #C1
	uses: (T2 + T1 + T3 withPrecedenceOf: T2)
	slots: {  }
	classVariables: {  }
	package: ''TraitsV2-Tests-TestClasses''' ])
]

{ #category : #tests }
T2PrecedenceCompositionTest >> testPrecedencesAreGeneratedInClassDefinitionWithAlias [
	| t1 t2 t3 c1 |
	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t3 := self newTrait: #T3 with: #() uses: #().
	t3 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	t3 compile: 'm2 ^ 11'.

	c1 := self newClass: #C1 with: #() uses: (t2 + (t1 -- #aSlot) + t3 withPrecedenceOf: t2).

	self
		assert: c1 definition
		equals:
			(Slot showSlotClassDefinition
				ifFalse: [ 'Object subclass: #C1
	uses: (T2 + (T1 -- #aSlot) + T3 withPrecedenceOf: T2)
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''TraitsV2-Tests-TestClasses''' ]
				ifTrue: [ 'Object subclass: #C1
	uses: (T2 + (T1 -- #aSlot) + T3 withPrecedenceOf: T2)
	slots: {  }
	classVariables: {  }
	package: ''TraitsV2-Tests-TestClasses''' ])
]

{ #category : #tests }
T2PrecedenceCompositionTest >> testPrecedencesCanBeCombined [

	| t1 t2 t3 c1 |

	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t3 := self newTrait: #T3 with: #() uses: #().
	t3 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.

	t3 compile: 'm2 ^ 11'.

	c1 := self newClass: #C1 with: #() uses: (((t2 + t1) withPrecedenceOf: t2) + t3).
	
	self assert: (c1 new m1) equals: 33.	
	self assert: (c1 new m2) equals: 11.
]

{ #category : #tests }
T2PrecedenceCompositionTest >> testWithPrecedenceIsNonAConflict [

	| t1 t2 c1 |

	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	
	c1 := self newClass: #C1 with: #() uses: ((t1 + t2) withPrecedenceOf: t2).

	self deny: (c1 >> #m1) isConflict
]

{ #category : #tests }
T2PrecedenceCompositionTest >> testWithPrecedenceUsesThePreferedOne [

	| t1 t2 c1 |

	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	
	c1 := self newClass: #C1 with: #() uses: ((t1 + t2) withPrecedenceOf: t2).

	self assert: (c1 new m1) equals: 33.
]

{ #category : #tests }
T2PrecedenceCompositionTest >> testWithPrecedenceUsesThePreferedOneWithoutCaringOrderOfSequence [

	| t1 t2 c1 |

	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	
	c1 := self newClass: #C1 with: #() uses: ((t2 + t1) withPrecedenceOf: t2).

	self assert: (c1 new m1) equals: 33.
]

{ #category : #tests }
T2PrecedenceCompositionTest >> testWithoutPrecedenceIsAConflict [

	| t1 t2 c1 |

	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #().

	t2 := self newTrait: #T2 with: #() uses: #().
	t2 classTrait uses: {} slots: #().

	t1 compile: 'm1 ^ 42'.
	t2 compile: 'm1 ^ 33'.
	
	c1 := self newClass: #C1 with: #() uses: t1 + t2.

	self assert: (c1 >> #m1) isConflict
]
