"
I am a cluster for storing metaclasses in full detail.
"
Class {
	#name : 'FLMetaclassCluster',
	#superclass : 'FLBehaviorCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'analyzing' }
FLMetaclassCluster class >> clusterBucketIn: aClusterization [

	^aClusterization metalevelClassSideBucket
]

{ #category : 'private' }
FLMetaclassCluster >> instanceVariablesStringFor: aClass [
	^ aClass instanceVariablesString
]

{ #category : 'serialize/materialize' }
FLMetaclassCluster >> materializeInstanceWith: aDecoder [
	^Metaclass new
]

{ #category : 'serialize/materialize' }
FLMetaclassCluster >> materializePostInstance: aMetaclass with: aDecoder [

	| superclass instanceVariableNames format layout |
	superclass := aDecoder nextEncodedReference.
	instanceVariableNames := aDecoder nextEncodedString substrings: ' '.
	format := aDecoder nextEncodedUint24.
	layout := self materializeLayoutWith: aDecoder.

	aMetaclass
		basicSuperclass: superclass;
		methodDictionary: MethodDictionary new.
	
	self
		installLayout: layout
		withFormat: format
		variableNames: instanceVariableNames
		into: aMetaclass
]

{ #category : 'analyzing' }
FLMetaclassCluster >> referencesOf: aMetaclass do: aBlock [
	super
		referencesOf: aMetaclass
		do: aBlock.
	
	aBlock
		value: aMetaclass classLayout class;
		value: aMetaclass superclass
]

{ #category : 'serialize/materialize' }
FLMetaclassCluster >> serializeInstancesStepWith: anEncoder [
	"Do nothing. I know my objects have nothing to serialize in this step."
]

{ #category : 'serialize/materialize' }
FLMetaclassCluster >> serializePostInstance: aMetaclass with: anEncoder [

	anEncoder encodeReferenceTo: aMetaclass superclass.
	anEncoder encodeString: (self instanceVariablesStringFor: aMetaclass).
	anEncoder encodeUint24: aMetaclass format.
	self
		serializeLayout: aMetaclass classLayout
		with: anEncoder
]
