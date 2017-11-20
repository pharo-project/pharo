"
The purpose of my hierarchy is to map objects with clusters.
"
Class {
	#name : #FLMapper,
	#superclass : #Object,
	#instVars : [
		'next',
		'analysis'
	],
	#category : #Fuel-Mappers
}

{ #category : #accessing }
FLMapper >> analysis: anAnalysis [

	analysis := anAnalysis.
	next ifNotNil: [ next analysis: anAnalysis ]
]

{ #category : #protected-mapping }
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

{ #category : #protected-mapping }
FLMapper >> clusterKeyedByClusterName: aClusterClass [

	^ self
		clusterKeyedByClusterName: aClusterClass 
		factory:  [ aClusterClass newAnalyzing ]
]

{ #category : #protected-mapping }
FLMapper >> clusterKeyedByClusterName: aClusterClass factory: clusterFactory [

	^ self
		clusterInstanceOf: aClusterClass
		keyInBucket: aClusterClass name
		factory: clusterFactory

]

{ #category : #protected-mapping }
FLMapper >> clusterKeyedByObjectClass: clusterClass class: objectClass [

	^ self 
		clusterInstanceOf: clusterClass 
		keyInBucket: objectClass 
		factory: [ clusterClass newAnalyzing: objectClass ]
]

{ #category : #hooks }
FLMapper >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #mapping }
FLMapper >> mapAndTrace: anObject [

	self subclassResponsibility
]

{ #category : #protected-mapping }
FLMapper >> mapAndTraceByClusterName: anObject to: aClusterClass [

	(self clusterKeyedByClusterName: aClusterClass)
		add: anObject traceWith: analysis

]

{ #category : #protected-mapping }
FLMapper >> mapAndTraceByObjectClass: anObject to: aClusterClass [

	(self clusterKeyedByObjectClass: aClusterClass class: anObject class)
		add: anObject traceWith: analysis

]

{ #category : #accessing }
FLMapper >> next: anObject [
	next := anObject
]
