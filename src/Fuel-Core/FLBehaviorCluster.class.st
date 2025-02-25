"
I have the common behavior for serializing class and trait behaviors in full detail.
"
Class {
	#name : 'FLBehaviorCluster',
	#superclass : 'FLIteratingCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> afterMaterializationStepWith: aDecoder [
	objects do:	[ :aClassOrTrait |
		aClassOrTrait
			fuelRecomputeSlotScopeWithTraits;
			fuelInitializeTraitComposition ]
]

{ #category : 'serialize/materialize layouts' }
FLBehaviorCluster >> installLayout: aLayout withFormat: anInteger variableNames: variableNames into: aClassOrTrait [
	| scope superSlotSize slots |	
	aLayout hasSlotScope ifTrue: [
		superSlotSize := self superclassSlotSizeOf: aClassOrTrait.
		slots := variableNames withIndexCollect: [ :var :index |
			(InstanceVariableSlot named: var asSymbol)
				index: index + superSlotSize;
				yourself ].
		scope := (self superclassLayoutOf: aClassOrTrait) slotScope extend: slots.
		aLayout slotScope: scope ].
	
	aLayout host: aClassOrTrait.
	"NOTE: the class format may change later when we materialize trait variables.
		However, it is important to set the format correctly (or possibly too large) here,
		even though it may indicate more instance variables than the layout defines.
		If we don't do this, it will not be possible to store the additional variables from
		traits in the instances because the instances will not have been allocated with
		enough fields.
		See Behavior>>fuelRecomputeSlotScopeWithTraits
		and FLVariablesMapping>>materializeTraitVariables."
	aClassOrTrait
		classLayout: aLayout;
		"We have to make sure that instances are allocated with enough space
		when instance variables have been added in the target environment."
		setFormat: (anInteger max: aLayout format)
]

{ #category : 'serialize/materialize layouts' }
FLBehaviorCluster >> materializeLayoutWith: aDecoder [

	^ aDecoder nextEncodedReference new
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> materializePostInstance: aClassOrTrait with: aDecoder [

	self subclassResponsibility
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> materializePostInstancesStepWith: aDecoder [

	objects do:	[ :aClassOrTrait | self materializePostInstance: aClassOrTrait with: aDecoder ]
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> materializeReferencesOf: aClassOrTrait with: aDecoder [

	aClassOrTrait
		methodDictionary: aDecoder nextEncodedReference;
		protocols: aDecoder nextEncodedReference
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> materializeReferencesStepWith: aDecoder [

	objects do:	[ :aClassOrTrait | self materializeReferencesOf: aClassOrTrait with: aDecoder ]
]

{ #category : 'analyzing' }
FLBehaviorCluster >> referencesOf: aClassOrTrait do: aBlock [

	aBlock
		value: aClassOrTrait methodDictionary;
		value: aClassOrTrait protocols
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> registerIndexesOn: aDictionary [
	"Since the introduction of slots we need the superclass of materialized
	class to be fully materialized already when we materialize the slots.
	To ensure this we order classes here by the number of their superclasses.
	See #testCreateHierarchyWithSubclassSerializedBeforeSuperclass"
	objects := objects sorted: [ :a :b |
		a allSuperclasses size <= b allSuperclasses size ].
	
	super registerIndexesOn: aDictionary
]

{ #category : 'serialize/materialize layouts' }
FLBehaviorCluster >> serializeLayout: aLayout with: anEncoder [

	anEncoder encodeReferenceTo: aLayout class.
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> serializePostInstance: aClassOrTrait with: anEncoder [

	self subclassResponsibility
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> serializePostInstancesStepWith: anEncoder [  

	objects do:	[ :aClassOrTrait | self serializePostInstance: aClassOrTrait with: anEncoder ]
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> serializeReferencesOf: aClassOrTrait with: anEncoder [

	anEncoder
		encodeReferenceTo: aClassOrTrait methodDictionary;
		encodeReferenceTo: aClassOrTrait protocols
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> serializeReferencesStepWith: anEncoder [

	objects do:	[ :aClassOrTrait | self serializeReferencesOf: aClassOrTrait with: anEncoder ]
]

{ #category : 'serialize/materialize' }
FLBehaviorCluster >> setTag: packageTagName ofPackage: packageName to: aClass [

	packageName = UndefinedPackage undefinedPackageName ifFalse: [
		aClass packageOrganizer
			packageNamed: packageName
			ifPresent: [ :package |
				^ aClass basicTag: (package ensureTag: packageTagName) ] ].

	aClass basicTag: aClass packageOrganizer undefinedPackage undefinedTag
]

{ #category : 'serialize/materialize layouts' }
FLBehaviorCluster >> superclassLayoutOf: aClassOrTrait [
	^ aClassOrTrait superclass
			ifNotNil: [ :theSuperclass | theSuperclass classLayout ]
			ifNil: [
				"It's a trait"
				EmptyLayout instance]
]

{ #category : 'serialize/materialize layouts' }
FLBehaviorCluster >> superclassSlotSizeOf: aClassOrTrait [
	^ aClassOrTrait superclass
		ifNotNil: [ :theSuperclass | theSuperclass allSlots size ]
		ifNil: [
			"It's a trait"
			0 ]
]
