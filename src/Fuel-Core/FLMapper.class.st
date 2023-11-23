"
The purpose of my hierarchy is to map objects with clusters.
"
Class {
	#name : 'FLMapper',
	#superclass : 'Object',
	#traits : 'TFLConfigurable',
	#classTraits : 'TFLConfigurable classTrait',
	#instVars : [
		'next',
		'analysis'
	],
	#category : 'Fuel-Core-Mappers',
	#package : 'Fuel-Core',
	#tag : 'Mappers'
}

{ #category : 'protected-mapping' }
FLMapper >> clusterInstanceOf: aClusterClass keyInBucket: clusterKey factory: clusterFactory [

	| bucket |
	bucket := aClusterClass clusterBucketIn: analysis clusterization.
	^ bucket
		at: clusterKey
		ifAbsentPut: [ 
			clusterFactory value 
				traceWith: analysis;
				yourself ].
]

{ #category : 'protected-mapping' }
FLMapper >> clusterKeyedByClusterName: aClusterClass [

	^ self
		clusterKeyedByClusterName: aClusterClass 
		factory:  [ aClusterClass newAnalyzing ]
]

{ #category : 'protected-mapping' }
FLMapper >> clusterKeyedByClusterName: aClusterClass factory: clusterFactory [

	^ self
		clusterInstanceOf: aClusterClass
		keyInBucket: aClusterClass name
		factory: clusterFactory
]

{ #category : 'protected-mapping' }
FLMapper >> clusterKeyedByObjectClass: clusterClass class: objectClass [

	^ self 
		clusterInstanceOf: clusterClass 
		keyInBucket: objectClass 
		factory: [ clusterClass newAnalyzing: objectClass ]
]

{ #category : 'initialization' }
FLMapper >> initialize [
	super initialize.
	
	analysis := self context analysis
]

{ #category : 'mapping' }
FLMapper >> mapAndTrace: anObject [

	self subclassResponsibility
]

{ #category : 'protected-mapping' }
FLMapper >> mapAndTraceByClusterName: anObject to: aClusterClass [

	(self clusterKeyedByClusterName: aClusterClass)
		add: anObject traceWith: analysis
]

{ #category : 'protected-mapping' }
FLMapper >> mapAndTraceByObjectClass: anObject to: aClusterClass [

	(self clusterKeyedByObjectClass: aClusterClass class: anObject class)
		add: anObject traceWith: analysis
]

{ #category : 'accessing' }
FLMapper >> next: anObject [
	next := anObject
]

{ #category : 'visiting' }
FLMapper >> visitSubstitution: anObject by: aSubstitute [
	next
		visitSubstitution: anObject
		by: aSubstitute
]
