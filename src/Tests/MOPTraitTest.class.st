Class {
	#name : #MOPTraitTest,
	#superclass : #TestCase,
	#traits : 'THelperTraitTest',
	#classTraits : 'THelperTraitTest classTrait',
	#category : #'Tests-Traits-MOP'
}

{ #category : #running }
MOPTraitTest >> setUp [
	super setUp.
	
	self generateTraitForTests.
	self generateTestClasses.
]

{ #category : #running }
MOPTraitTest >> tearDown [
	
	self removeTraitForTests.
	self removeTestClasses.
	super tearDown. 
]

{ #category : #tests }
MOPTraitTest >> testClass [	
	"The class of a compiled method is the class that contains it.
	A compiled method is shared."
	"methodClass could call 
		-> methodClassOrTrait"
	
	self assert: (trait1>>#c) methodClass equals: trait1.
	self assert: (trait2>>#c) methodClass equals: trait2.
	self assert: (testClassC>>#c) methodClass equals: testClassC.
	self assert: (testClassC>>#c2) methodClass equals: testClassC.
	self assert: (testClassD>>#c) methodClass equals: testClassD.
	self assert: (testClassD>>#c2) methodClass equals: testClassD.
	self assert: (testClassD>>#c3) methodClass equals: testClassD.
	self assert: (testClassA>>#c2) methodClass equals: testClassA.
	
]

{ #category : #tests }
MOPTraitTest >> testOrigin [

	"The origin of a compiledMethod is its defining class or trait."

	| aliasedMethod |
	self assert: (testClassC >> #c) origin equals: testClassC.
	self assert: (testClassA >> #c) origin equals: trait3.
	self assert: (trait3 >> #c2) origin equals: trait2.
	self assert: (testClassA >> #c2) origin equals: trait2.
	self assert: (testClassB >> #c) origin equals: trait1.
	self assert: (testClassD >> #c3) origin equals: trait2.
	self assert: (testClassD >> #c2) origin equals: trait2.

	aliasedMethod := testClassD >> #c3.
	self
		assert: aliasedMethod origin
		identicalTo: aliasedMethod originMethod methodClass
]

{ #category : #tests }
MOPTraitTest >> testSelector [	
	"The selector of a compiled method should be its name. 
	An aliased method should have the name of its alias name."
	
	self assert: (testClassA>>#c) selector equals: #c.
	self assert: (testClassC>>#c) selector equals: #c.
	self assert: (trait3>>#c) selector equals: #c.
	self assert: (trait3>>#c2) selector equals: #c2.
	self assert: (testClassD>>#c3) selector equals: #c3.
]
