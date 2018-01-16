Class {
	#name : #RG2EnvironmentTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2EnvironmentTest >> testBehaviortNamed [

	| env behavior | 
	
	env := RG2Environment new.
	behavior := env ensureClassNamed: 'SomeClass'.
	self assert: (env ask behaviorNamed: 'SomeClass') equals: behavior.

	env := RG2Environment new.
	behavior := env ensureClassNamed: 'SomeClass class'.
	self assert: (env ask behaviorNamed: 'SomeClass class') equals: behavior.
	
	env := RG2Environment new.
	behavior := env ensureClassNamed: 'SomeTrait'.
	self assert: (env ask behaviorNamed: 'SomeTrait') equals: behavior.
	
	env := RG2Environment new.
	behavior := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (env ask behaviorNamed: 'SomeTrait classTrait') equals: behavior.
	
	env := RG2Environment new.
	self assert: (env ask behaviorNamed: 'NonexistingBehavior') equals: nil.
	self assert: (env ask behaviorNamed: 'NonexistingBehavior class') equals: nil.
	self assert: (env ask behaviorNamed: 'NonexistingBehavior classTrait') equals: nil.

]

{ #category : #tests }
RG2EnvironmentTest >> testBehaviortNamedIndirect [

	| env behavior | 
	
	env := RG2Environment new.
	behavior := env ensureClassNamed: 'SomeClass'.
	self assert: (env ask behaviorNamed: 'SomeClass') equals: behavior.
	self assert: (env ask behaviorNamed: 'SomeClass class') equals: behavior metaclass.
	self assert: (env ask behaviorNamed: 'SomeClass classTrait') equals: nil.

	env := RG2Environment new.
	behavior := env ensureClassNamed: 'SomeClass class'.
	self assert: (env ask behaviorNamed: 'SomeClass class') equals: behavior.
	self assert: (env ask behaviorNamed: 'SomeClass') equals: behavior baseClass.
	self assert: (env ask behaviorNamed: 'SomeClass classTrait') equals: nil.

	env := RG2Environment new.
	behavior := env ensureTraitNamed: 'SomeTrait'.
	self assert: (env ask behaviorNamed: 'SomeTrait') equals: behavior.
	self assert: (env ask behaviorNamed: 'SomeTrait classTrait') equals: behavior classTrait.
	self assert: (env ask behaviorNamed: 'SomeTrait class') equals: nil.

	env := RG2Environment new.
	behavior := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (env ask behaviorNamed: 'SomeTrait classTrait') equals: behavior.
	self assert: (env ask behaviorNamed: 'SomeTrait') equals: behavior baseTrait.
	self assert: (env ask behaviorNamed: 'SomeTrait class') equals: nil.

]

{ #category : #tests }
RG2EnvironmentTest >> testCleanUnusedUnreferencedBehaviors [

	| env class1 class2 | 
	
	env := RG2Environment new.
	class1 := env ensureClassNamed: 'SomeClass1'.
	class2 := env ensureClassNamed: 'SomeClass2'.
	self assert: env ask behaviors size equals: 8. "only pre-check"
	
	"class1 superclass makeResolved.
	class1 metaclass makeResolved.	
	class1 superclass metaclass makeResolved."
	
	class2 superclass: class1 superclass.
	class2 metaclass superclass: class1 metaclass superclass.
	class2 metaclass metaclass: class1 metaclass metaclass.
	self assert: env ask behaviors size equals: 8. "nothing changed"
	
	env cleanUnusedUnreferencedBehaviors.
	self assert: env ask behaviors size equals: 6. 
	
	
	
	

]

{ #category : #tests }
RG2EnvironmentTest >> testCleanUnusedUnreferencedPackages [

	| env package1 package2 | 
	
	env := RG2Environment new.
	
	self assert: env ask packages isEmpty.
	env cleanUnusedUnreferencedPackages.
	self assert: env ask packages isEmpty.

	env ensurePackageNamed: #SomePackage.
	self assert: env ask packages size equals: 1.
	env cleanUnusedUnreferencedPackages.
	self assert: env ask packages isEmpty.
	
	(env ensureClassNamed: #SomeClass)
		package: (package1 := env ensurePackageNamed: #SomeUsedPackage).
	package2 := env ensurePackageNamed: #SomeUnusedPackage.
	self assert: (env ask packages includes: package1).  
	self assert: (env ask packages includes: package2).  
	env cleanUnusedUnreferencedPackages.
	self assert: (env ask packages includes: package1).  
	self deny: (env ask packages includes: package2).  
	
]

{ #category : #tests }
RG2EnvironmentTest >> testCompatibility [

	| env1 env2 |
	
	"TODO: needs rewriting"
	
"	env1 := RG2Environment named: 'env1'.
	env2 := RG2Environment named: 'env2'.	
	self deny: (env1 compatibleWith: env2).

	env1 := RG2Environment named: 'env'.
	env2 := RG2Environment named: 'env'.	
	self assert: (env1 compatibleWith: env2).

	env1 := RG2Environment unnamed.
	env2 := RG2Environment unnamed.
	self assert: (env1 compatibleWith: env2).
	
	env1 := RG2Environment named: 'env1'.
	env2 := RG2Environment unnamed.
	self deny: (env1 compatibleWith: env2).

	env1 := RG2Environment unnamed.
	env2 := RG2Environment named: 'env2'.
	self deny: (env1 compatibleWith: env2).		
			
	env1 := RG2Environment unnamed.
	env1 cleanBehaviors.
	env2 := RG2Environment unnamed.
	env2 cleanBehaviors.
	self assert: (env1 compatibleWith: env2).

"

]

{ #category : #tests }
RG2EnvironmentTest >> testDefaultEnvironment [

	| env  | 
	
	env := RG2Environment new.
	env createDefaultEnvironment.
	
	self assert: (env ask behaviors collect: #name) asSortedCollection equals: #(#Behavior #'Behavior class' #Class #'Class class' #ClassDescription #'ClassDescription class' #Metaclass #'Metaclass class' #Object #'Object class' #ProtoObject #'ProtoObject class') asSortedCollection.
]

{ #category : #tests }
RG2EnvironmentTest >> testEnsureClassNamed [

	| env definition | 
	
	env := RG2Environment new.
	definition := env ensurePackageNamed: 'SomePackage'.
	self assert: definition isPackage. 

	env := RG2Environment new.
	definition := env ensureClassNamed: 'SomeClass'.
	self assert: definition isClass. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait not. 	
	self assert: definition isMetaclassTrait not. 	

	env := RG2Environment new.
	definition := env ensureClassNamed: 'SomeClass class'.
	self assert: definition isClass not. 
	self assert: definition isMetaclass. 	
	self assert: definition isTrait not. 	
	self assert: definition isMetaclassTrait not. 	


]

{ #category : #tests }
RG2EnvironmentTest >> testEnsureTraitNamed [

	| env definition existing | 
	
	env := RG2Environment new.
	definition := env ensureTraitNamed: 'SomeTrait'.
	self assert: definition isClass not. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait. 	
	self assert: definition isMetaclassTrait not. 	

	env := RG2Environment new.
	definition := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: definition isClass not. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait. 	
	self assert: definition isMetaclassTrait. 	

	env := RG2Environment new.
	existing := (env ensureTraitNamed: 'SomeTrait classTrait') baseTrait.
	self assert: (existing isTrait).
	self assert: (existing isRing2Resolved not).
	definition := env ensureTraitNamed: 'SomeTrait'.
	self assert: (definition == existing).
	self assert: definition isClass not. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait. 	
	self assert: definition isMetaclassTrait not. 	

	env := RG2Environment new.
	existing := (env ensureClassNamed: 'SomeTrait').
	self assert: (existing isClass).
	self assert: (existing isRing2Resolved).
	definition := env ensureTraitNamed: 'SomeTrait'.
	self assert: (definition == existing).
	self assert: definition isClass not. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait. 	
	self assert: definition isMetaclassTrait not. 	

	env := RG2Environment new.
	existing := (env ensureTraitNamed: 'SomeTrait') classTrait.
	self assert: (existing isMetaclassTrait).
	self assert: (existing isRing2Resolved not).
	definition := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (definition == existing).
	self assert: definition isClass not. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait. 	
	self assert: definition isMetaclassTrait. 	
	self assert: definition baseTrait name equals: #SomeTrait.

	env := RG2Environment new.
	existing := (env ensureClassNamed: 'SomeTrait') metaclass.
	self assert: (existing isMetaclass).
	self assert: (existing isRing2Resolved not).
	definition := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (definition == existing).
	self assert: definition isClass not. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait. 	
	self assert: definition isMetaclassTrait. 	
	self assert: definition baseTrait name equals: #SomeTrait.

	env := RG2Environment new.
	existing := (env ensureClassNamed: 'SomeTrait class') baseClass .
	self assert: (existing isClass).
	self assert: (existing isRing2Resolved not).
	definition := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (definition baseTrait == existing).
	self assert: definition baseTrait isClass not. 
	self assert: definition baseTrait isMetaclass not. 	
	self assert: definition baseTrait isTrait. 	
	self assert: definition baseTrait isMetaclassTrait not. 	
	self assert: definition baseTrait name equals: #SomeTrait.

]

{ #category : #tests }
RG2EnvironmentTest >> testEnvironmentWithNamespace [

	| newEnvironment newPackage|
	
	newEnvironment := RG2Environment named: 'Testing environment'.
	
	self assert: (newEnvironment isRing2Resolved).
	
	newPackage := RG2PackageDefinition named: 'Collections-Sequenceable'.
	
	self assert: (newPackage isRing2Resolved).
	self deny: (newPackage isRing2FullyResolved).
	self deny: (newPackage isRing2FullyUnresolved).
	newPackage cleanDefinedBehaviors.
	newPackage cleanExtensionMethods.
	newPackage cleanTagsForClasses.
	self assert: (newPackage isRing2Resolved).
	self assert: (newPackage isRing2FullyResolved).
	self deny: (newPackage isRing2FullyUnresolved).
	
	self should: [newEnvironment addPackage: newPackage] raise: AssertionFailure.
]

{ #category : #tests }
RG2EnvironmentTest >> testFixProtoObjectClassSuperclass [

	| env protoObject class | 
	
	env := RG2Environment new.
	protoObject := env ensureClassNamed: 'ProtoObject'.
	class := env ensureClassNamed: 'Class'.

	self deny: (protoObject classSide superclass = class).
	env fixProtoObjectClassSuperclass.
	self assert: (protoObject classSide superclass = class).
]

{ #category : #tests }
RG2EnvironmentTest >> testMerging [

"	| env1 env2 |
	
	env1 := RG2Environment named: 'env1'.
	env2 := RG2Environment named: 'env2'.	
	self deny: (env1 canMergeSimplyWith: env2).
	
	env1 := RG2Environment unnamed.
	env2 := RG2Environment unnamed.
	self deny: (env1 canMergeSimplyWith: env2).
	
	env1 := RG2Environment named: 'env1'.
	env2 := RG2Environment unnamed.
	self deny: (env1 canMergeSimplyWith: env2).

	env1 := RG2Environment unnamed.
	env2 := RG2Environment named: 'env2'.
	self deny: (env1 canMergeSimplyWith: env2).		
			
	env1 := RG2Environment unnamed.
	env1 cleanPackages.
	env2 := RG2Environment unnamed.
	env2 cleanPackages.
	self deny: (env1 canMergeSimplyWith: env2).


"
]

{ #category : #tests }
RG2EnvironmentTest >> testNewEnvironment [
	| newEnvironment |
	
	newEnvironment := RG2Environment named: 'Testing environment'.
	
	self assert: (newEnvironment isRing2Resolved).
	
	self assert: (newEnvironment isEnvironment).
	self assert: (newEnvironment name = 'Testing environment').
	self assert: (newEnvironment ask behaviors isEmpty).
	self assert: (newEnvironment hasResolved: #name).
	self assert: (newEnvironment hasUnresolved: #behaviors).
	self assert: (newEnvironment hasUnresolved: #globalVariables).
	
	newEnvironment cleanPackages.
	newEnvironment cleanBehaviors.
	newEnvironment cleanGlobalVariables.
	
	self assert: (newEnvironment unresolvedProperties isEmpty).
	self assert: (newEnvironment ask packages isEmpty).

		

]

{ #category : #tests }
RG2EnvironmentTest >> testPackageNamed [

	| env package | 
	
	env := RG2Environment new.
	package := env ensurePackageNamed: 'SomePackage'.
	self assert: (env ask packageNamed: 'SomePackage') equals: package.
	
	env := RG2Environment new.
	self assert: (env ask packageNamed: 'SomePackage') equals: nil.
]

{ #category : #tests }
RG2EnvironmentTest >> testReferencedPackages [

	| env | 
	
	env := RG2Environment new.
	"
	self assert: env referencedPackages isEmpty."
]

{ #category : #tests }
RG2EnvironmentTest >> testTraitNamed [

	| env trait | 
	
	env := RG2Environment new.
	trait := env ensureTraitNamed: 'SomeTrait'.
	self assert: (env ask traitNamed: 'SomeTrait') equals: trait.
	
	env := RG2Environment new.
	trait := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (env ask traitNamed: 'SomeTrait classTrait') equals: trait.
	
	env := RG2Environment new.
	trait := env ensureClassNamed: 'SomeTrait'.
	self assert: (env ask traitNamed: 'SomeTrait') equals: nil.
]

{ #category : #tests }
RG2EnvironmentTest >> testUnifyClassTrait [

	| env trait1 trait2 classTrait| 
	
	env := RG2Environment new.
	trait1 := env ensureTraitNamed: 'SomeTrait1'.
	trait2 := env ensureTraitNamed: 'SomeTrait2'.
	
	self deny: (trait1 classTrait metaclass = trait2 classTrait metaclass).
	env unifyClassTrait.
	self assert: (trait1 classTrait metaclass = trait2 classTrait metaclass).

	env := RG2Environment new.
	trait1 := env ensureTraitNamed: 'SomeTrait1'.
	trait2 := env ensureTraitNamed: 'SomeTrait2'.
	classTrait := env ensureClassNamed: 'ClassTrait'.
	
	self deny: (trait1 classTrait metaclass = trait2 classTrait metaclass).
	self deny: (trait1 classTrait metaclass = classTrait).
	env unifyClassTrait.
	self assert: (trait1 classTrait metaclass = trait2 classTrait metaclass).
	self assert: (trait1 classTrait metaclass = classTrait).


]

{ #category : #tests }
RG2EnvironmentTest >> testUnifyMetaclass [

	| env class1 class2 metaclass| 
	
	env := RG2Environment new.
	class1 := env ensureClassNamed: 'SomeClass1'.
	class2 := env ensureClassNamed: 'SomeClass2'.
	
	self deny: (class1 metaclass metaclass = class2 metaclass metaclass).
	env unifyMetaclass.
	self assert: (class1 metaclass metaclass = class2 metaclass metaclass).

	env := RG2Environment new.
	class1 := env ensureClassNamed: 'SomeClass1'.
	class2 := env ensureClassNamed: 'SomeClass2'.
	metaclass := env ensureTraitNamed: 'Metaclass'.
	
	self deny: (class1 metaclass metaclass = class2 metaclass metaclass).
	self deny: (class1 metaclass metaclass = metaclass).
	env unifyMetaclass.
	self assert: (class1 metaclass metaclass = class2 metaclass metaclass).
	self assert: (class1 metaclass metaclass = metaclass).


]

{ #category : #tests }
RG2EnvironmentTest >> testUnifyTrait [

	| env trait1 trait2 trait| 
	
	env := RG2Environment new.
	trait1 := env ensureTraitNamed: 'SomeTrait1'.
	trait2 := env ensureTraitNamed: 'SomeTrait2'.
	
	self deny: (trait1 metaclass = trait2 metaclass).
	env unifyTrait.
	self assert: (trait1 metaclass = trait2 metaclass).

	env := RG2Environment new.
	trait1 := env ensureTraitNamed: 'SomeTrait1'.
	trait2 := env ensureTraitNamed: 'SomeTrait2'.
	trait := env ensureClassNamed: 'Trait'.
	
	self deny: (trait1 metaclass = trait2 metaclass).
	self deny: (trait1 metaclass = trait).
	env unifyTrait.
	self assert: (trait1 metaclass = trait2 metaclass).
	self assert: (trait1 metaclass = trait).


]

{ #category : #tests }
RG2EnvironmentTest >> testUnpackagedPackage [

	| env package | 
	
	env := RG2Environment new.
	self assert: env unpackagedPackageOrNil equals: nil.
	package := env ensurePackageNamed: '_UnpackagedPackage'.
	
	self assert: env unpackagedPackageOrNil equals: package.
	
	
]
