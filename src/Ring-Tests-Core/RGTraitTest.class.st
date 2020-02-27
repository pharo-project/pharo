Class {
	#name : #RGTraitTest,
	#superclass : #RGTest,
	#traits : 'TRGBehaviorTest',
	#classTraits : 'TRGBehaviorTest classTrait',
	#category : #'Ring-Tests-Core'
}

{ #category : #'as yet unclassified' }
RGTraitTest >> behaviorClass [

	^ RGTrait
]

{ #category : #tests }
RGTraitTest >> testClassSide [
	| behavior |
	behavior := RGTrait unnamed.
	self assert: behavior classSide identicalTo: behavior classTrait.
	self assert: behavior classTrait classSide identicalTo: behavior classTrait
]

{ #category : #'as yet unclassified' }
RGTraitTest >> testEnsureProtocol [

	| behavior |
	
	behavior := self behaviorClass named: #SomeClass.
	behavior ensureProtocolNamed: #SomeProtocol.
	self assert: behavior protocols size equals: 1.
	self assert: behavior protocols first equals: #SomeProtocol.
	
]

{ #category : #tests }
RGTraitTest >> testNewTrait [
	| trait metaclassTrait comment category package |
	trait := RGTrait unnamed.
	self assert: trait isRingResolved.
	self assert: (trait hasUnresolvedAll: #(classTrait comment category package)).
	self assert: trait isTrait.
	self assert: trait trait identicalTo: trait.

	metaclassTrait := RGMetaclassTrait parent: trait environment.
	trait classTrait: metaclassTrait.
	self assert: (trait hasResolved: #classTrait).
	self assert: trait classTrait identicalTo: metaclassTrait.
	self assert: trait theMetaClass identicalTo: metaclassTrait.

	comment := RGComment parent: metaclassTrait.
	self deny: trait hasComment.
	trait comment: comment.
	self assert: (trait hasResolved: #comment).
	self assert: trait comment identicalTo: comment.
	self deny: trait hasComment.	"the comment has no content"
	comment content: 'some comment'.
	self assert: trait hasComment.

	category := #SomeCategory.
	trait category: category.
	self assert: (trait hasResolved: #tags).
	self assert: trait category identicalTo: category.

	package := RGPackage parent: trait environment.
	trait package: package.
	self assert: (trait hasResolved: #package).
	self assert: trait package identicalTo: package.

	trait classVarNames isEmpty.
	trait sharedPools isEmpty
]

{ #category : #tests }
RGTraitTest >> testTraitImplicitEnvironment [

	| trait |
	
	trait := RGTrait unnamed.
	self assert: (trait hasUnresolved: #name).
	self checkImplicitSingleTraitEnvironmentOf: trait.
	
	
	
	

]
