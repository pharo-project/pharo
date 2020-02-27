Class {
	#name : #RGBehaviorStrategy,
	#superclass : #Object,
	#instVars : [
		'owner'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'instance creation' }
RGBehaviorStrategy class >> unresolved [

	^ self basicNew
]

{ #category : #visiting }
RGBehaviorStrategy >> acceptVisitor: aVisitor [

	self subclassResponsibility 
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> addClassVariable: anRGInstanceVariableDefinition [ 

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> addSharedPool: anRGPoolVariable [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> allClassVarNames [

	self subclassResponsibility 
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> allClassVariables [

	self incompatibleBehaviorType
]

{ #category : #utilities }
RGBehaviorStrategy >> allSlots [

	^ OrderedCollection new
]

{ #category : #utilities }
RGBehaviorStrategy >> announceDefinitionChangeDuring: aBlock [

	self owner announceDefinitionChangeDuring: aBlock
]

{ #category : #utilities }
RGBehaviorStrategy >> backend [

	^ self owner backend forBehavior
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> baseBehavior [

	^ self owner
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> baseClass [

	^ self owner
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> baseClass: anRGClass [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> baseTrait [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> baseTrait: anRGClass [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> bindingOf: varName [

	^ self owner isRootInEnvironment
		ifFalse: [self owner superclass bindingOf: varName]
		ifTrue: [ nil ]
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> category [

	self subclassResponsibility 
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> category: aString [

	self incompatibleBehaviorType
]

{ #category : #accessing }
RGBehaviorStrategy >> classSide [

	^ self owner
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> classTrait [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> classTrait: anRGMetaclassTrait [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> classVarNames [

	^#()
]

{ #category : #'class variables' }
RGBehaviorStrategy >> classVariableDefinitionString [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> classVariables [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> classVariables: aCollectionOfSymbols [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> classVariablesDo: aBlock [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> classVariablesString [

	^ String new
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> cleanClassVariables [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> cleanSharedPools [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> comment [

	self subclassResponsibility
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> comment: anRGComment [

	self incompatibleBehaviorType
]

{ #category : #copying }
RGBehaviorStrategy >> copyForBehaviorDefinitionPostCopy [

	super postCopy.

]

{ #category : #copying }
RGBehaviorStrategy >> copyForBehaviorDefinitionWithOwner: aNewOwner [

	^ self shallowCopy 
		owner: aNewOwner;
		copyForBehaviorDefinitionPostCopy
]

{ #category : #'default model values' }
RGBehaviorStrategy >> defaultMetaClass [

	^ self incompatibleBehaviorType
]

{ #category : #utilities }
RGBehaviorStrategy >> environment [

	^ self owner environment
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> incompatibleBehaviorType [

	RGIncompatibleBehaviorTypeError signal
]

{ #category : #initialization }
RGBehaviorStrategy >> initializeUnresolved [
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> innerBindingOf: aSymbol [

	^ nil
]

{ #category : #utilities }
RGBehaviorStrategy >> instSize [

	^ 0
]

{ #category : #utilities }
RGBehaviorStrategy >> instVarNames [

	^ OrderedCollection new
]

{ #category : #'accessing - parallel hierarchy' }
RGBehaviorStrategy >> instanceSide [

	^ self owner
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> instanceVariables: aCollectionOfSymbols [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> instanceVariablesString [

	^ String new
]

{ #category : #testing }
RGBehaviorStrategy >> isClass [

	^ false
]

{ #category : #testing }
RGBehaviorStrategy >> isClassStrategy [

	^ false
]

{ #category : #testing }
RGBehaviorStrategy >> isMeta [
	"By default a non-meta class is considered"

	^false
]

{ #category : #testing }
RGBehaviorStrategy >> isMetaclass [
	
	^ false
]

{ #category : #testing }
RGBehaviorStrategy >> isMetaclassStrategy [

	^ false
]

{ #category : #testing }
RGBehaviorStrategy >> isMetaclassTrait [
	
	^ false
]

{ #category : #testing }
RGBehaviorStrategy >> isMetaclassTraitStrategy [

	^ false
]

{ #category : #testing }
RGBehaviorStrategy >> isTrait [

	^false
]

{ #category : #testing }
RGBehaviorStrategy >> isTraitStrategy [

	^ false
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> kindOfSubclass [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> makeResolved [

	"do nothing"

]

{ #category : #accessing }
RGBehaviorStrategy >> owner [
	^ owner
]

{ #category : #accessing }
RGBehaviorStrategy >> owner: anObject [
	owner := anObject
]

{ #category : #utilities }
RGBehaviorStrategy >> pvtResolvableProperties [

	^ OrderedCollection new
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> referencedBehaviors [

	^ { self owner superclass. self owner metaclass}, self owner traitComposition referencedBehaviors
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> referencedPackages [

	^ Array new
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> removeClassVariable: anRGInstanceVariableDefinition [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> removeSharedPool: anRGPoolVariable [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> sharedPoolNames [

	^ OrderedCollection new
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> sharedPools [

	^ OrderedCollection new
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> sharedPools: aCollectionOfSymbols [

	self incompatibleBehaviorType
]

{ #category : #'private - backend access' }
RGBehaviorStrategy >> sharedPoolsDo: aBlock [

	self incompatibleBehaviorType
]

{ #category : #utilities }
RGBehaviorStrategy >> sibling [

	^ self subclassResponsibility 
]

{ #category : #utilities }
RGBehaviorStrategy >> storeName [

	self subclassResponsibility 
]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGBehaviorStrategy >> theMetaClass [

	^ self owner
]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGBehaviorStrategy >> theNonMetaClass [

	^ self owner
]

{ #category : #variables }
RGBehaviorStrategy >> trait [

	^ self incompatibleBehaviorType
]

{ #category : #utilities }
RGBehaviorStrategy >> unresolvedValue: aDefaultValue [

	^ self owner unresolvedValue: aDefaultValue
]
