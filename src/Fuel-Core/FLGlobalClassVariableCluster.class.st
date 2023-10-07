"
I am a cluster for class variable associations, i.e. bindings that belong to the class pool of a Class included on Smalltalk globals.
"
Class {
	#name : 'FLGlobalClassVariableCluster',
	#superclass : 'FLIteratingCluster',
	#category : 'Fuel-Core-Clusters-Global',
	#package : 'Fuel-Core',
	#tag : 'Clusters-Global'
}

{ #category : 'analyzing' }
FLGlobalClassVariableCluster class >> clusterBucketIn: aClusterization [

	^aClusterization baselevelBucket
]

{ #category : 'serialize/materialize' }
FLGlobalClassVariableCluster >> materializeInstanceWith: aDecoder [

	| keyName ownerClass |
	ownerClass := aDecoder nextEncodedReference.
	keyName := aDecoder nextEncodedReference.
	^ownerClass classPool associationAt: keyName
]

{ #category : 'analyzing' }
FLGlobalClassVariableCluster >> referencesOf: aClassVariable do: aBlock [
	aBlock 
		value: aClassVariable owningClass;
		value: aClassVariable key
]

{ #category : 'serialize/materialize' }
FLGlobalClassVariableCluster >> serializeInstance: aClassVariable with: anEncoder [
	anEncoder 
		encodeReferenceTo: aClassVariable owningClass;
		encodeReferenceTo: aClassVariable key
]
