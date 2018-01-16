Class {
	#name : #RG2TraitDefinitionTest,
	#superclass : #RG2Test,
	#traits : 'TRG2BehaviorDefinitionTest',
	#classTraits : 'TRG2BehaviorDefinitionTest classTrait',
	#category : #'Ring2-Tests-Core'
}

{ #category : #'as yet unclassified' }
RG2TraitDefinitionTest >> behaviorClass [

	^ RG2TraitDefinition
]

{ #category : #tests }
RG2TraitDefinitionTest >> testClassSide [
	
	| behavior |
	behavior := RG2TraitDefinition unnamed.
	self assert: behavior classSide == behavior classTrait. 
	self assert: behavior classTrait classSide == behavior classTrait. 
	
]

{ #category : #'as yet unclassified' }
RG2TraitDefinitionTest >> testEnsureProtocol [

	| behavior protocol |
	
	behavior := self behaviorClass named: #SomeClass.
	behavior ensureProtocolNamed: #SomeProtocol.
	self assert: behavior protocols size equals: 1.
	self assert: behavior protocols first equals: #SomeProtocol.
	
]

{ #category : #tests }
RG2TraitDefinitionTest >> testNewTrait [

	| trait metaclassTrait comment category package | 
	
	trait  := RG2TraitDefinition unnamed.
	self assert: (trait isRing2Resolved).
	self assert: (trait hasUnresolvedAll: #(classTrait comment category package)).
	self assert: (trait isTrait).
	self assert: (trait trait == trait).

	metaclassTrait  := RG2MetaclassTraitDefinition parent: trait environment.
	trait classTrait: metaclassTrait.
	self assert: (trait hasResolved: #classTrait).
	self assert: (trait classTrait == metaclassTrait).
	self assert: (trait theMetaClass == metaclassTrait).

	comment := RG2CommentDefinition parent: metaclassTrait. 
	self deny: (trait hasComment).
	trait comment:  comment.
	self assert: (trait hasResolved: #comment).
	self assert: (trait comment == comment).
	self deny: (trait hasComment). "the comment has no content"
	comment content: 'some comment'.
	self assert: (trait hasComment).

	category  := #'SomeCategory'.
	trait category: category.
	self assert: (trait hasResolved: #tags).
	self assert: (trait category == category).

	package  := RG2PackageDefinition parent: trait environment.
	trait package:  package.
	self assert: (trait hasResolved: #package).
	self assert: (trait package == package).
	
	trait classVarNames isEmpty.
	trait sharedPools isEmpty.
		
]

{ #category : #tests }
RG2TraitDefinitionTest >> testTraitImplicitEnvironment [

	| trait |
	
	trait := RG2TraitDefinition unnamed.
	self assert: (trait hasUnresolved: #name).
	self checkImplicitSingleTraitEnvironmentOf: trait.
	
	
	
	

]
