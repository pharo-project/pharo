"
I am an abstract behavior that has a behavior strategy and and delegates all messages to it
"
Class {
	#name : #RGBehaviorStrategyUser,
	#superclass : #RGObject,
	#instVars : [
		'behaviorStrategy'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #visiting }
RGBehaviorStrategyUser >> acceptVisitor: aVisitor [ 
	
	^ self behaviorStrategy acceptVisitor: aVisitor
]

{ #category : #strategy }
RGBehaviorStrategyUser >> addClassVariable: anRGInstanceVariableDefinition [ 

	^ self behaviorStrategy addClassVariable: anRGInstanceVariableDefinition 
]

{ #category : #strategy }
RGBehaviorStrategyUser >> addSharedPool: anRGPoolVariable [

	^ self behaviorStrategy addSharedPool: anRGPoolVariable
]

{ #category : #strategy }
RGBehaviorStrategyUser >> allClassVarNames [

	^ self behaviorStrategy allClassVarNames
]

{ #category : #strategy }
RGBehaviorStrategyUser >> allClassVariables [

	^ self behaviorStrategy allClassVariables
]

{ #category : #'as yet unclassified' }
RGBehaviorStrategyUser >> allSlots [

	^ self behaviorStrategy allSlots
]

{ #category : #'queries - other' }
RGBehaviorStrategyUser >> baseBehavior [

	^ self behaviorStrategy baseBehavior
]

{ #category : #'as yet unclassified' }
RGBehaviorStrategyUser >> baseClass [

	^ self behaviorStrategy baseClass
]

{ #category : #accessing }
RGBehaviorStrategyUser >> baseClass: anRGClass [

	^ self behaviorStrategy baseClass: anRGClass
]

{ #category : #strategy }
RGBehaviorStrategyUser >> baseTrait [

	^ self behaviorStrategy baseTrait
]

{ #category : #strategy }
RGBehaviorStrategyUser >> baseTrait: anRGClass [

	^ self behaviorStrategy baseTrait: anRGClass
]

{ #category : #accessing }
RGBehaviorStrategyUser >> behaviorStrategy [
	^ behaviorStrategy
]

{ #category : #accessing }
RGBehaviorStrategyUser >> behaviorStrategy: anObject [

	| needsInitialization |
	
	needsInitialization := behaviorStrategy isNil.
	behaviorStrategy := anObject.
	behaviorStrategy owner: self.
	needsInitialization ifTrue: [ 
		behaviorStrategy initializeUnresolved ].
	
]

{ #category : #strategy }
RGBehaviorStrategyUser >> bindingOf: varName [

	^ self behaviorStrategy bindingOf: varName

]

{ #category : #strategy }
RGBehaviorStrategyUser >> category [

	^ self behaviorStrategy category
]

{ #category : #strategy }
RGBehaviorStrategyUser >> category: aString [

	^ self behaviorStrategy category: aString
]

{ #category : #strategy }
RGBehaviorStrategyUser >> classSide [

	^ self behaviorStrategy classSide
]

{ #category : #strategy }
RGBehaviorStrategyUser >> classTrait [

	^ self behaviorStrategy classTrait
]

{ #category : #strategy }
RGBehaviorStrategyUser >> classTrait: anRGMetaclassTrait [

	^ self behaviorStrategy classTrait: anRGMetaclassTrait
]

{ #category : #strategy }
RGBehaviorStrategyUser >> classVarNames [

	^ self behaviorStrategy classVarNames
]

{ #category : #strategy }
RGBehaviorStrategyUser >> classVariableDefinitionString [

	^ self behaviorStrategy classVariableDefinitionString
]

{ #category : #strategy }
RGBehaviorStrategyUser >> classVariables [

	^ self behaviorStrategy classVariables
]

{ #category : #strategy }
RGBehaviorStrategyUser >> classVariables: aCollectionOfSymbols [

	^ self behaviorStrategy classVariables: aCollectionOfSymbols
]

{ #category : #strategy }
RGBehaviorStrategyUser >> classVariablesDo: aBlock [

	^ self behaviorStrategy classVariablesDo: aBlock
]

{ #category : #strategy }
RGBehaviorStrategyUser >> cleanClassVariables [

	^ self behaviorStrategy cleanClassVariables
]

{ #category : #strategy }
RGBehaviorStrategyUser >> cleanSharedPools [

	^ self behaviorStrategy cleanSharedPools
]

{ #category : #strategy }
RGBehaviorStrategyUser >> comment [

	^ self behaviorStrategy comment
]

{ #category : #strategy }
RGBehaviorStrategyUser >> comment: anRGComment [

	^ self behaviorStrategy comment: anRGComment
]

{ #category : #strategy }
RGBehaviorStrategyUser >> defaultMetaClass [

	^ self behaviorStrategy defaultMetaClass
]

{ #category : #'accessing - definition' }
RGBehaviorStrategyUser >> definition [

	^ self behaviorStrategy definition
]

{ #category : #strategy }
RGBehaviorStrategyUser >> innerBindingOf: aSymbol [

	^ self behaviorStrategy innerBindingOf: aSymbol
]

{ #category : #'as yet unclassified' }
RGBehaviorStrategyUser >> instSize [

	^ self behaviorStrategy instSize
]

{ #category : #'as yet unclassified' }
RGBehaviorStrategyUser >> instVarNames [
	^ self behaviorStrategy instVarNames
]

{ #category : #'as yet unclassified' }
RGBehaviorStrategyUser >> instanceSide [

	^ self behaviorStrategy instanceSide
]

{ #category : #accessing }
RGBehaviorStrategyUser >> instanceVariables: aCollectionOfSymbols [

	^ self behaviorStrategy instanceVariables: aCollectionOfSymbols
]

{ #category : #printing }
RGBehaviorStrategyUser >> instanceVariablesString [

	^ self behaviorStrategy instanceVariablesString
]

{ #category : #testing }
RGBehaviorStrategyUser >> isClass [

	^ self behaviorStrategy isClass
]

{ #category : #'queries - testing' }
RGBehaviorStrategyUser >> isMeta [
	"By default a non-meta class is considered"

	^ self behaviorStrategy isMeta
]

{ #category : #'testing types' }
RGBehaviorStrategyUser >> isMetaclass [

	^ self behaviorStrategy isMetaclass
]

{ #category : #'testing types' }
RGBehaviorStrategyUser >> isMetaclassTrait [

	^ self behaviorStrategy isMetaclassTrait
]

{ #category : #strategy }
RGBehaviorStrategyUser >> isTrait [

	^ self behaviorStrategy isTrait
]

{ #category : #'testing class hierarchy' }
RGBehaviorStrategyUser >> kindOfSubclass [

	^ self behaviorStrategy kindOfSubclass
]

{ #category : #strategy }
RGBehaviorStrategyUser >> layout [

	^ self behaviorStrategy layout
]

{ #category : #strategy }
RGBehaviorStrategyUser >> layout: anRGLayout [

	^ self behaviorStrategy layout: anRGLayout
]

{ #category : #resolving }
RGBehaviorStrategyUser >> makeResolved [

	self behaviorStrategy makeResolved.
	super makeResolved.

]

{ #category : #strategy }
RGBehaviorStrategyUser >> package [

	^ self behaviorStrategy package
]

{ #category : #strategy }
RGBehaviorStrategyUser >> package: anRGPackage [

	^ self behaviorStrategy package: anRGPackage

]

{ #category : #strategy }
RGBehaviorStrategyUser >> referencedBehaviors [

	^ self behaviorStrategy referencedBehaviors
]

{ #category : #strategy }
RGBehaviorStrategyUser >> referencedPackages [

	^ self behaviorStrategy referencedPackages
]

{ #category : #strategy }
RGBehaviorStrategyUser >> removeClassVariable: anRGInstanceVariableDefinition [

	^ self behaviorStrategy removeClassVariable: anRGInstanceVariableDefinition
]

{ #category : #strategy }
RGBehaviorStrategyUser >> removeSharedPool: anRGPoolVariable [

	^ self behaviorStrategy removeSharedPool: anRGPoolVariable
]

{ #category : #testing }
RGBehaviorStrategyUser >> sharedPoolNames [

	^ self behaviorStrategy sharedPoolNames
]

{ #category : #strategy }
RGBehaviorStrategyUser >> sharedPools [

	^ self behaviorStrategy sharedPools
]

{ #category : #strategy }
RGBehaviorStrategyUser >> sharedPools: aCollectionOfSymbols [

	^ self behaviorStrategy sharedPools: aCollectionOfSymbols
]

{ #category : #strategy }
RGBehaviorStrategyUser >> sharedPoolsDo: aBlock [

	^ self behaviorStrategy sharedPoolsDo: aBlock
]

{ #category : #strategy }
RGBehaviorStrategyUser >> sibling [

	^ self behaviorStrategy sibling
]

{ #category : #printing }
RGBehaviorStrategyUser >> storeOn: aStream [ 
	
	^ self behaviorStrategy storeOn: aStream
]

{ #category : #'as yet unclassified' }
RGBehaviorStrategyUser >> theMetaClass [

	^ self behaviorStrategy theMetaClass
]

{ #category : #'as yet unclassified' }
RGBehaviorStrategyUser >> theNonMetaClass [
	
	^ self behaviorStrategy theNonMetaClass
]

{ #category : #variables }
RGBehaviorStrategyUser >> trait [

	"Because of API compatibility between traits and trait transformations"

	^ self behaviorStrategy trait
]

{ #category : #strategy }
RGBehaviorStrategyUser >> traitTransformationString [

	^ self behaviorStrategy traitTransformationString
]
