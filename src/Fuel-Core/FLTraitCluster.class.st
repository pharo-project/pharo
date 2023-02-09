"
I am a cluster for storing traits in full detail.
"
Class {
	#name : #FLTraitCluster,
	#superclass : #FLBehaviorCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLTraitCluster class >> clusterBucketIn: aClusterization [

	^aClusterization metalevelInstanceSideBucket
]

{ #category : #'serialize/materialize' }
FLTraitCluster >> materializeInstanceWith: aDecoder [
	| traitClass |
	traitClass := aDecoder nextEncodedReference.
	^ traitClass new
]

{ #category : #'serialize/materialize' }
FLTraitCluster >> materializePostInstance: aTrait with: aDecoder [
	| category name environment format layout instanceVariableNames |
	category := aDecoder nextEncodedReference.
	name := aDecoder nextEncodedReference.
	environment := aDecoder nextEncodedReference.
	format := aDecoder nextEncodedReference.
	layout := self materializeLayoutWith: aDecoder.
	instanceVariableNames := aDecoder nextEncodedString substrings: ' '.
	
	aTrait 
		initialize;
		"#initialize sets Object as the superclass
		but the superclass of traits is nil"
		superclass: nil;
		setName: name;
		basicCategory: (category ifNil: [ RPackage defaultPackageName ]) asSymbol;
		environment: environment.
	self
		installLayout: layout
		withFormat: format
		variableNames: instanceVariableNames
		into: aTrait
]

{ #category : #analyzing }
FLTraitCluster >> referencesOf: aTrait do: aBlock [
	super referencesOf: aTrait do: aBlock.

	aBlock
		value: aTrait classSide;
		value: aTrait basicEnvironment;
		value: aTrait category;
		value: aTrait name;
		value: aTrait format;
		value: (FLPlatform current hasStatefulTraits
			ifTrue: [ aTrait classLayout class ]
			ifFalse: [ nil ])
]

{ #category : #'serialize/materialize' }
FLTraitCluster >> serializeInstance: aTrait with: anEncoder [
	anEncoder encodeReferenceTo: aTrait classSide
]

{ #category : #'serialize/materialize' }
FLTraitCluster >> serializePostInstance: aTrait with: anEncoder [
		"Do not use #basicCategory because since it may be lazily computed it may happen that it is still in nil. "
	anEncoder
		encodeReferenceTo: aTrait category;
		encodeReferenceTo: aTrait name;
		encodeReferenceTo: aTrait basicEnvironment;
		encodeReferenceTo: aTrait format.
	FLPlatform current hasStatefulTraits
		ifTrue: [
			self
				serializeLayout: aTrait classLayout
				with: anEncoder ]
		ifFalse: [ anEncoder encodeReferenceTo: nil ].
	anEncoder encodeString: aTrait instanceVariablesString
]
