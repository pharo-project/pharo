Class {
	#name : #MOPTraitTest,
	#superclass : #TestCase,
	#category : #'Tests-Traits-MOP'
}

{ #category : #tests }
MOPTraitTest >> testClass [	
	"The class of a compiled method is the class that contains it.
	A compiled method is shared."
	"methodClass could call 
		-> methodClassOrTrait"
	
	self assert: (Trait1>>#c) methodClass equals: Trait1.
	self assert: (Trait2>>#c) methodClass equals: Trait2.
	self assert: (MOPTestClassC>>#c) methodClass equals: MOPTestClassC.
	self assert: (MOPTestClassC>>#c2) methodClass equals: MOPTestClassC.
	self assert: (MOPTestClassD>>#c) methodClass equals: MOPTestClassD.
	self assert: (MOPTestClassD>>#c2) methodClass equals: MOPTestClassD.
	self assert: (MOPTestClassD>>#c3) methodClass equals: MOPTestClassD.
	self assert: (MOPTestClassA>>#c2) methodClass equals: MOPTestClassA.
	
]

{ #category : #tests }
MOPTraitTest >> testOrigin [
	"The origin of a compiledMethod is its defining class or trait."

	| aliasedMethod |
	self assert: (MOPTestClassC >> #c) origin equals: MOPTestClassC.
	self assert: (MOPTestClassA >> #c) origin equals: Trait3.
	self assert: (Trait3 >> #c2) origin equals: Trait2.
	self assert: (MOPTestClassA >> #c2) origin equals: Trait2.
	self assert: (MOPTestClassB >> #c) origin equals: Trait1.
	self assert: (MOPTestClassD >> #c3) origin equals: Trait2.
	self assert: (MOPTestClassD >> #c2) origin equals: Trait2.

	aliasedMethod := MOPTestClassD >> #c3.
	self assert: aliasedMethod origin identicalTo: aliasedMethod originMethod methodClass
]

{ #category : #tests }
MOPTraitTest >> testSelector [	
	"The selector of a compiled method should be its name. 
	An aliased method should have the name of its alias name."
	
	self assert: (MOPTestClassA>>#c) selector equals: #c.
	self assert: (MOPTestClassC>>#c) selector equals: #c.
	self assert: (Trait3>>#c) selector equals: #c.
	self assert: (Trait3>>#c2) selector equals: #c2.
	self assert: (MOPTestClassD>>#c3) selector equals: #c3.
]
