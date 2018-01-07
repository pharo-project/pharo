Class {
	#name : #RG2Test,
	#superclass : #TestCase,
	#category : #'Ring2-Tests-Core'
}

{ #category : #utilities }
RG2Test >> checkHierarchyConsistencyOf: aRG2ClassDefinition [

	self assert: (aRG2ClassDefinition ~~ aRG2ClassDefinition superclass).
	self assert: (aRG2ClassDefinition ~~ aRG2ClassDefinition metaclass).
	self assert: (aRG2ClassDefinition ~~ aRG2ClassDefinition superclass metaclass).

	self assert: (aRG2ClassDefinition superclass ~~ aRG2ClassDefinition metaclass).
	self assert: (aRG2ClassDefinition superclass ~~ aRG2ClassDefinition superclass metaclass).
	self assert: (aRG2ClassDefinition metaclass ~~ aRG2ClassDefinition superclass metaclass).

	self assert: (aRG2ClassDefinition superclass superclass == aRG2ClassDefinition superclass).
	self assert: (aRG2ClassDefinition superclass metaclass == aRG2ClassDefinition metaclass superclass).

	self assert: (aRG2ClassDefinition superclass metaclass superclass == aRG2ClassDefinition superclass metaclass).
	self assert: (aRG2ClassDefinition superclass metaclass metaclass == aRG2ClassDefinition superclass metaclass).
	

]

{ #category : #utilities }
RG2Test >> checkImplicitSingleClassEnvironmentOf: aRG2ClassDefinition [
	
	self deny: (aRG2ClassDefinition isRing2Resolved).

	"we have SomeClass, SomeClass class, SomeClass superclass, SomeClass superclass class"
	self assert: aRG2ClassDefinition environment ask behaviors size equals: 4.

	"we have one package for SomeClass and SomeClass class, other for SomeClass superclass 
	and SomeClass superclass class"	
	self assert: aRG2ClassDefinition environment ask packages size equals: 2.

	self checkHierarchyConsistencyOf: aRG2ClassDefinition.
	
	
	

]

{ #category : #utilities }
RG2Test >> checkImplicitSingleTraitEnvironmentOf: aRG2TraitDefinition [
	
	self assert: aRG2TraitDefinition environment ask behaviors size equals: 10.
	self assert: aRG2TraitDefinition environment ask packages size equals: 5.

	self checkHierarchyConsistencyOf: aRG2TraitDefinition metaclass.
	
	self assert: (aRG2TraitDefinition ~~ aRG2TraitDefinition classTrait).
	self assert: (aRG2TraitDefinition superclass == aRG2TraitDefinition superclass).
	self assert: (aRG2TraitDefinition classTrait superclass == aRG2TraitDefinition classTrait superclass).
	
	
	
	

]
