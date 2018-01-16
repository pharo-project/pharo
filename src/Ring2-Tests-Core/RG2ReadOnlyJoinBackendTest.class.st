Class {
	#name : #RG2ReadOnlyJoinBackendTest,
	#superclass : #RG2BackendTest,
	#traits : 'TRG2ReadOnlyDefinitionTest',
	#classTraits : 'TRG2ReadOnlyDefinitionTest classTrait',
	#category : #'Ring2-Tests-Core'
}

{ #category : #'as yet unclassified' }
RG2ReadOnlyJoinBackendTest >> joinedEnvironmentFor: aRGEnvironment1 and: aRGEnvironment2 [

	| joinEnv |
	
	joinEnv := RG2Environment new.
	joinEnv backend: (RG2ReadOnlyJoinBackend for: joinEnv).
	joinEnv backend environment1: aRGEnvironment1.
	joinEnv backend environment2: aRGEnvironment2.
	
	^ joinEnv 
]

{ #category : #'as yet unclassified' }
RG2ReadOnlyJoinBackendTest >> testJoinEnvironmentsWithDifferentBehaviors [

	| class1 class2 joinEnv |
	
	class1 := RG2ClassDefinition named: #SomeClass1.
	class2 := RG2ClassDefinition named: #SomeClass2.

	joinEnv := self joinedEnvironmentFor: class1 environment and: class2 environment.

	self assert: (joinEnv ask behaviorNamed: #SomeClass1) notNil.
	self assert: (joinEnv ask behaviorNamed: #SomeClass2) notNil.	
		
	self assert: joinEnv ask behaviors size equals: 8.


]

{ #category : #'as yet unclassified' }
RG2ReadOnlyJoinBackendTest >> testJoinEnvironmentsWithOverlappedHierarchy [

	| class1 superclass1 class2 joinEnv found1 found2 |
	
	class1 := RG2ClassDefinition named: #SomeClass.
	superclass1 := class1 environment ensureClassNamed: #Object.
	class1 superclass: superclass1.
	
	class2 := RG2MetaclassDefinition named: #Object.

	joinEnv := self joinedEnvironmentFor: class1 environment and: class2 environment.

	found1 := joinEnv ask behaviorNamed: #SomeClass.
	found2 := joinEnv ask behaviorNamed: #Object.
	
	self assert: found1 notNil.
	self assert: found2 notNil.	
		
	self assert: found1 ~~ class1.
	self assert: found2 ~~ class2.

	self assert: found1 environment equals: joinEnv.
	self assert: found2 environment equals: joinEnv.
	
	found1 metaclass.
	
	self assert: joinEnv ask behaviors size equals: 8.


]

{ #category : #tests }
RG2ReadOnlyJoinBackendTest >> testJoinEnvironmentsWithSameBehaviors [

	| class1 class2 joinEnv |
	
	class1 := RG2ClassDefinition named: #SomeClass.
	class1 superclass: (class1 environment ensureClassNamed: #SomeClassSuperclass). 
	
	class2 := RG2ClassDefinition named: #SomeClass.
	class2 superclass: (class2 environment ensureClassNamed: #SomeClassOtherSuperclass). 

	joinEnv := self joinedEnvironmentFor: class1 environment and: class2 environment.
	
	self should: [ joinEnv ask behaviors ] raise: RG2JoiningError.

]

{ #category : #'as yet unclassified' }
RG2ReadOnlyJoinBackendTest >> testJoinWithImageEnvironment [

	| env class1 superclass1 joinEnv found |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	class1 := RG2ClassDefinition named: #SomeClass.
	superclass1 := class1 environment ensureClassNamed: #Object.
	class1 superclass: superclass1.
	class1 environment clean.

	joinEnv := RG2Environment new.
	joinEnv backend: (RG2ReadOnlyJoinBackend for: joinEnv).
	joinEnv backend environment1: env.
	joinEnv backend environment2: class1 environment.

	self shouldnt: [found := joinEnv ask behaviorNamed: #SomeClass ] raise: RG2JoiningError.
	
	self assert: found notNil.
	self assert: found superclass name equals: #Object.
	self assert: found superclass superclass name equals: #ProtoObject.
	

]
