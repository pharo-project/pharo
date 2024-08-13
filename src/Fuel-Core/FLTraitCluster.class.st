"
I am a cluster for storing traits in full detail.
"
Class {
	#name : 'FLTraitCluster',
	#superclass : 'FLBehaviorCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'analyzing' }
FLTraitCluster class >> clusterBucketIn: aClusterization [

	^aClusterization metalevelInstanceSideBucket
]

{ #category : 'serialize/materialize' }
FLTraitCluster >> materializeInstanceWith: aDecoder [
	| traitClass |
	traitClass := aDecoder nextEncodedReference.
	^ traitClass new
]

{ #category : 'serialize/materialize' }
FLTraitCluster >> materializePostInstance: aTrait with: aDecoder [

	| name environment format layout instanceVariableNames comment stamp packageName packageTagName |
	packageName := aDecoder nextEncodedReference.
	packageTagName := aDecoder nextEncodedReference.
	name := aDecoder nextEncodedReference.
	environment := aDecoder nextEncodedReference.
	format := aDecoder nextEncodedReference.
	layout := self materializeLayoutWith: aDecoder.
	instanceVariableNames := aDecoder nextEncodedString substrings: ' '.
	comment := aDecoder nextEncodedString.
	stamp := aDecoder nextEncodedString.

	aTrait
		initialize;
		"#initialize sets Object as the superclass
		but the superclass of traits is nil"
		superclass: nil;
		setName: name;
		environment: environment.

	self setTag: packageTagName ofPackage: packageName to: aTrait.

	self
		installLayout: layout
		withFormat: format
		variableNames: instanceVariableNames
		into: aTrait.

	aTrait comment: comment stamp: stamp
]

{ #category : 'analyzing' }
FLTraitCluster >> referencesOf: aTrait do: aBlock [

	super referencesOf: aTrait do: aBlock.

	aBlock
		value: aTrait classSide;
		value: aTrait basicEnvironment;
		value: aTrait package name;
		value: aTrait packageTag name;
		value: aTrait name;
		value: aTrait format;
		value: aTrait classLayout class
]

{ #category : 'serialize/materialize' }
FLTraitCluster >> serializeInstance: aTrait with: anEncoder [
	anEncoder encodeReferenceTo: aTrait classSide
]

{ #category : 'serialize/materialize' }
FLTraitCluster >> serializePostInstance: aTrait with: anEncoder [

	anEncoder
		encodeReferenceTo: aTrait package name;
		encodeReferenceTo: aTrait packageTag name;
		encodeReferenceTo: aTrait name;
		encodeReferenceTo: aTrait basicEnvironment;
		encodeReferenceTo: aTrait format.

	self serializeLayout: aTrait classLayout with: anEncoder.

	anEncoder
		encodeString: aTrait instanceVariablesString;
		encodeString: aTrait comment;
		encodeString: aTrait commentStamp
]
