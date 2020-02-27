Class {
	#name : #RGEnvironmentQueryInterfaceTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGEnvironmentQueryInterfaceTest >> testCachingBehaviors [

	| env behavior |
	
	env := RGEnvironment new.
	self assert: (env ask behaviorsDictionary isEmpty).
	behavior := env ensureClassNamed: #SomeBehavior.
	self assert: (env ask behaviorsDictionary at: #SomeBehavior) equals: behavior.
	self assert: (env ask includesClassNamed: #SomeBehavior).
	
	self assert: (env ask allClasses notEmpty).
	self assert: (env ask allClassesAndTraits notEmpty).
	
	env ask invalidateName: #SomeBehavior.
	self assert: (env ask behaviorsDictionary isEmpty).

	env := RGEnvironment new.
	self assert: (env ask behaviorsDictionary isEmpty).
	behavior := env ensureClassNamed: #SomeBehavior.
	self assert: (env ask behaviorsDictionary at: #SomeBehavior) equals: behavior.
	env ask resetBehaviorsDictionary.
	self assert: (env ask behaviorsDictionary isEmpty).

	env := RGEnvironment new.
	self assert: (env ask behaviorsDictionary isEmpty).
	behavior := env ensureClassNamed: #SomeBehavior.
	self assert: (env ask behaviorsDictionary at: #SomeBehavior) equals: behavior.
	"change class to trait"
	behavior := env ensureTraitNamed: #SomeBehavior.
	self assert: (env ask behaviorsDictionary at: #SomeBehavior) equals: behavior.

	
]
