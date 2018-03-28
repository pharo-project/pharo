"
I am a cluster for objects that are reachables from Smalltalk global dictionary.
"
Class {
	#name : #FLGlobalCluster,
	#superclass : #FLPrimitiveCluster,
	#category : 'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLGlobalCluster class >> clusterBucketIn: aClusterization [

	^aClusterization globalsBucket 
]

{ #category : #'serialize/materialize' }
FLGlobalCluster >> materializeGlobalAssociationFrom: aDecoder [

	| globalName |
	globalName := aDecoder nextEncodedString asSymbol.
	^ aDecoder globalEnvironment
		associationAt: globalName
		ifAbsent: [ FLGlobalNotFound signalWithName: globalName ]

]

{ #category : #'serialize/materialize' }
FLGlobalCluster >> materializeGlobalClassFrom: aDecoder [

	| className |
	className := aDecoder nextEncodedString asSymbol.
	^ aDecoder classNamed: className
]

{ #category : #'serialize/materialize' }
FLGlobalCluster >> serializeGlobalAssociationKeyed: aSymbol on: anEncoder [
	
	anEncoder encodeString: aSymbol
]

{ #category : #'serialize/materialize' }
FLGlobalCluster >> serializeGlobalClass: aClass on: anEncoder [
	
	aClass isObsolete ifTrue: [ FLObsolete signalWithName: aClass name ].
	anEncoder encodeString: aClass name 
]
