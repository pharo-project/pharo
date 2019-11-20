Class {
	#name : #RGEnvironmentTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGEnvironmentTest >> testBehaviortNamed [

	| env behavior | 
	
	env := RGEnvironment new.
	behavior := env ensureClassNamed: 'SomeClass'.
	self assert: (env ask behaviorNamed: 'SomeClass') equals: behavior.

	env := RGEnvironment new.
	behavior := env ensureClassNamed: 'SomeClass class'.
	self assert: (env ask behaviorNamed: 'SomeClass class') equals: behavior.
	
	env := RGEnvironment new.
	behavior := env ensureClassNamed: 'SomeTrait'.
	self assert: (env ask behaviorNamed: 'SomeTrait') equals: behavior.
	
	env := RGEnvironment new.
	behavior := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (env ask behaviorNamed: 'SomeTrait classTrait') equals: behavior.
	
	env := RGEnvironment new.
	self assert: (env ask behaviorNamed: 'NonexistingBehavior') equals: nil.
	self assert: (env ask behaviorNamed: 'NonexistingBehavior class') equals: nil.
	self assert: (env ask behaviorNamed: 'NonexistingBehavior classTrait') equals: nil.

]

{ #category : #tests }
RGEnvironmentTest >> testBehaviortNamedIndirect [

	| env behavior | 
	
	env := RGEnvironment new.
	behavior := env ensureClassNamed: 'SomeClass'.
	self assert: (env ask behaviorNamed: 'SomeClass') equals: behavior.
	self assert: (env ask behaviorNamed: 'SomeClass class') equals: behavior metaclass.
	self assert: (env ask behaviorNamed: 'SomeClass classTrait') equals: nil.

	env := RGEnvironment new.
	behavior := env ensureClassNamed: 'SomeClass class'.
	self assert: (env ask behaviorNamed: 'SomeClass class') equals: behavior.
	self assert: (env ask behaviorNamed: 'SomeClass') equals: behavior baseClass.
	self assert: (env ask behaviorNamed: 'SomeClass classTrait') equals: nil.

	env := RGEnvironment new.
	behavior := env ensureTraitNamed: 'SomeTrait'.
	self assert: (env ask behaviorNamed: 'SomeTrait') equals: behavior.
	self assert: (env ask behaviorNamed: 'SomeTrait classTrait') equals: behavior classTrait.
	self assert: (env ask behaviorNamed: 'SomeTrait class') equals: nil.

	env := RGEnvironment new.
	behavior := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (env ask behaviorNamed: 'SomeTrait classTrait') equals: behavior.
	self assert: (env ask behaviorNamed: 'SomeTrait') equals: behavior baseTrait.
	self assert: (env ask behaviorNamed: 'SomeTrait class') equals: nil.

]

{ #category : #tests }
RGEnvironmentTest >> testCleanUnusedUnreferencedBehaviors [

	| env class1 class2 | 
	
	env := RGEnvironment new.
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
RGEnvironmentTest >> testCleanUnusedUnreferencedPackages [

	| env package1 package2 | 
	
	env := RGEnvironment new.
	
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
RGEnvironmentTest >> testCompatibility [

	"TODO: needs rewriting"
	
"	env1 := RGEnvironment named: 'env1'.
	env2 := RGEnvironment named: 'env2'.	
	self deny: (env1 compatibleWith: env2).

	env1 := RGEnvironment named: 'env'.
	env2 := RGEnvironment named: 'env'.	
	self assert: (env1 compatibleWith: env2).

	env1 := RGEnvironment unnamed.
	env2 := RGEnvironment unnamed.
	self assert: (env1 compatibleWith: env2).
	
	env1 := RGEnvironment named: 'env1'.
	env2 := RGEnvironment unnamed.
	self deny: (env1 compatibleWith: env2).

	env1 := RGEnvironment unnamed.
	env2 := RGEnvironment named: 'env2'.
	self deny: (env1 compatibleWith: env2).		
			
	env1 := RGEnvironment unnamed.
	env1 cleanBehaviors.
	env2 := RGEnvironment unnamed.
	env2 cleanBehaviors.
	self assert: (env1 compatibleWith: env2).

"

]

{ #category : #tests }
RGEnvironmentTest >> testDefaultEnvironment [

	| env  | 
	
	env := RGEnvironment new.
	env createDefaultEnvironment.
	
	self assert: (env ask behaviors collect: #name) asSortedCollection equals: #(#Behavior #'Behavior class' #Class #'Class class' #ClassDescription #'ClassDescription class' #Metaclass #'Metaclass class' #Object #'Object class' #ProtoObject #'ProtoObject class') asSortedCollection.
]

{ #category : #tests }
RGEnvironmentTest >> testEnsureClassNamed [

	| env definition | 
	
	env := RGEnvironment new.
	definition := env ensurePackageNamed: 'SomePackage'.
	self assert: definition isPackage. 

	env := RGEnvironment new.
	definition := env ensureClassNamed: 'SomeClass'.
	self assert: definition isClass. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait not. 	
	self assert: definition isMetaclassTrait not. 	

	env := RGEnvironment new.
	definition := env ensureClassNamed: 'SomeClass class'.
	self assert: definition isClass not. 
	self assert: definition isMetaclass. 	
	self assert: definition isTrait not. 	
	self assert: definition isMetaclassTrait not. 	


]

{ #category : #tests }
RGEnvironmentTest >> testEnvironmentWithNamespace [

	| newEnvironment newPackage|
	
	newEnvironment := RGEnvironment named: 'Testing environment'.
	
	self assert: (newEnvironment isRingResolved).
	
	newPackage := RGPackage named: 'Collections-Sequenceable'.
	
	self assert: (newPackage isRingResolved).
	self deny: (newPackage isRingFullyResolved).
	self deny: (newPackage isRingFullyUnresolved).
	newPackage cleanDefinedBehaviors.
	newPackage cleanExtensionMethods.
	newPackage cleanTagsForClasses.
	self assert: (newPackage isRingResolved).
	self assert: (newPackage isRingFullyResolved).
	self deny: (newPackage isRingFullyUnresolved).
	
	self should: [newEnvironment addPackage: newPackage] raise: RGWrongEnvironment.
]

{ #category : #tests }
RGEnvironmentTest >> testFixProtoObjectClassSuperclass [
	| env protoObject class |
	env := RGEnvironment new.
	protoObject := env ensureClassNamed: 'ProtoObject'.
	class := env ensureClassNamed: 'Class'.

	self deny: protoObject classSide superclass equals: class.
	env fixProtoObjectClassSuperclass.
	self assert: protoObject classSide superclass equals: class
]

{ #category : #tests }
RGEnvironmentTest >> testMerging [

"	| env1 env2 |
	
	env1 := RGEnvironment named: 'env1'.
	env2 := RGEnvironment named: 'env2'.	
	self deny: (env1 canMergeSimplyWith: env2).
	
	env1 := RGEnvironment unnamed.
	env2 := RGEnvironment unnamed.
	self deny: (env1 canMergeSimplyWith: env2).
	
	env1 := RGEnvironment named: 'env1'.
	env2 := RGEnvironment unnamed.
	self deny: (env1 canMergeSimplyWith: env2).

	env1 := RGEnvironment unnamed.
	env2 := RGEnvironment named: 'env2'.
	self deny: (env1 canMergeSimplyWith: env2).		
			
	env1 := RGEnvironment unnamed.
	env1 cleanPackages.
	env2 := RGEnvironment unnamed.
	env2 cleanPackages.
	self deny: (env1 canMergeSimplyWith: env2).


"
]

{ #category : #tests }
RGEnvironmentTest >> testNewEnvironment [
	| newEnvironment |
	newEnvironment := RGEnvironment named: 'Testing environment'.

	self assert: newEnvironment isRingResolved.

	self assert: newEnvironment isEnvironment.
	self assert: newEnvironment name equals: 'Testing environment'.
	self assert: newEnvironment ask behaviors isEmpty.
	self assert: newEnvironment hasResolvedName.
	self assert: (newEnvironment hasUnresolved: #behaviors).
	self assert: (newEnvironment hasUnresolved: #globalVariables).

	newEnvironment cleanPackages.
	newEnvironment cleanBehaviors.
	newEnvironment cleanGlobalVariables.

	self assert: newEnvironment unresolvedProperties isEmpty.
	self assert: newEnvironment ask packages isEmpty
]

{ #category : #tests }
RGEnvironmentTest >> testPackageNamed [

	| env package | 
	
	env := RGEnvironment new.
	package := env ensurePackageNamed: 'SomePackage'.
	self assert: (env ask packageNamed: 'SomePackage') equals: package.
	
	env := RGEnvironment new.
	self assert: (env ask packageNamed: 'SomePackage') equals: nil.
]

{ #category : #tests }
RGEnvironmentTest >> testReferencedPackages [

	| env | 
	
	env := RGEnvironment new.
	"
	self assert: env referencedPackages isEmpty."
]

{ #category : #tests }
RGEnvironmentTest >> testTraitNamed [

	| env trait | 
	
	env := RGEnvironment new.
	trait := env ensureTraitNamed: 'SomeTrait'.
	self assert: (env ask traitNamed: 'SomeTrait') equals: trait.
	
	env := RGEnvironment new.
	trait := env ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: (env ask traitNamed: 'SomeTrait classTrait') equals: trait.
	
	env := RGEnvironment new.
	trait := env ensureClassNamed: 'SomeTrait'.
	self assert: (env ask traitNamed: 'SomeTrait') equals: nil.
]

{ #category : #tests }
RGEnvironmentTest >> testUnifyClassTrait [
	| env trait1 trait2 classTrait |
	env := RGEnvironment new.
	trait1 := env ensureTraitNamed: 'SomeTrait1'.
	trait2 := env ensureTraitNamed: 'SomeTrait2'.

	self deny: trait1 classTrait metaclass equals: trait2 classTrait metaclass.
	env unifyClassTrait.
	self assert: trait1 classTrait metaclass equals: trait2 classTrait metaclass.

	env := RGEnvironment new.
	trait1 := env ensureTraitNamed: 'SomeTrait1'.
	trait2 := env ensureTraitNamed: 'SomeTrait2'.
	classTrait := env ensureClassNamed: 'ClassTrait'.

	self deny: trait1 classTrait metaclass equals: trait2 classTrait metaclass.
	self deny: trait1 classTrait metaclass equals: classTrait.
	env unifyClassTrait.
	self assert: trait1 classTrait metaclass equals: trait2 classTrait metaclass.
	self assert: trait1 classTrait metaclass equals: classTrait
]

{ #category : #tests }
RGEnvironmentTest >> testUnifyMetaclass [
	| env class1 class2 metaclass |
	env := RGEnvironment new.
	class1 := env ensureClassNamed: 'SomeClass1'.
	class2 := env ensureClassNamed: 'SomeClass2'.

	self deny: class1 metaclass metaclass equals: class2 metaclass metaclass.
	env unifyMetaclass.
	self assert: class1 metaclass metaclass equals: class2 metaclass metaclass.

	env := RGEnvironment new.
	class1 := env ensureClassNamed: 'SomeClass1'.
	class2 := env ensureClassNamed: 'SomeClass2'.
	metaclass := env ensureTraitNamed: 'Metaclass'.

	self deny: class1 metaclass metaclass equals: class2 metaclass metaclass.
	self deny: class1 metaclass metaclass equals: metaclass.
	env unifyMetaclass.
	self assert: class1 metaclass metaclass equals: class2 metaclass metaclass.
	self assert: class1 metaclass metaclass equals: metaclass
]

{ #category : #tests }
RGEnvironmentTest >> testUnifyTrait [
	| env trait1 trait2 trait |
	env := RGEnvironment new.
	trait1 := env ensureTraitNamed: 'SomeTrait1'.
	trait2 := env ensureTraitNamed: 'SomeTrait2'.

	self deny: trait1 metaclass equals: trait2 metaclass.
	env unifyTrait.
	self assert: trait1 metaclass equals: trait2 metaclass.

	env := RGEnvironment new.
	trait1 := env ensureTraitNamed: 'SomeTrait1'.
	trait2 := env ensureTraitNamed: 'SomeTrait2'.
	trait := env ensureClassNamed: 'Trait'.

	self deny: trait1 metaclass equals: trait2 metaclass.
	self deny: trait1 metaclass equals: trait.
	env unifyTrait.
	self assert: trait1 metaclass equals: trait2 metaclass.
	self assert: trait1 metaclass equals: trait
]

{ #category : #tests }
RGEnvironmentTest >> testUnpackagedPackage [

	| env package | 
	
	env := RGEnvironment new.
	self assert: env unpackagedPackageOrNil equals: nil.
	package := env ensurePackageNamed: '_UnpackagedPackage'.
	
	self assert: env unpackagedPackageOrNil equals: package.
	
	
]
