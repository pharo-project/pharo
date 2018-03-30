"
I map substituted objects. 

For example, suppose you want to substitute instances of WriteStream by nil. In such case, I will map every WriteStream instance to my substitutions cluster, and its factory block will value to nil with any argument.

"
Class {
	#name : #FLPluggableSubstitutionMapper,
	#superclass : #FLMapper,
	#instVars : [
		'condition',
		'substitutionFactory'
	],
	#category : 'Fuel-Core-Mappers'
}

{ #category : #'instance creation' }
FLPluggableSubstitutionMapper class >> when: aCondition substituteBy: aFactory [

	^self basicNew
		initializeWith: aCondition 
		substitutionFactory: aFactory;
		yourself
]

{ #category : #'initialize-release' }
FLPluggableSubstitutionMapper >> initializeWith: aCondition substitutionFactory: aBlock [

	self initialize.
	condition := aCondition.
	substitutionFactory := aBlock.
]

{ #category : #mapping }
FLPluggableSubstitutionMapper >> mapAndTrace: anObject [
	"Uses the Chain of Responsibility pattern to answer the cluster which maps with the received object."

	(condition value: anObject)
		ifTrue: [ self mapAndTraceSubstitutionIfNotRecursive: anObject ]
		ifFalse: [ next mapAndTrace: anObject ]
]

{ #category : #mapping }
FLPluggableSubstitutionMapper >> mapAndTraceSubstitutionIfNotRecursive: anObject [
		
	| cluster |
	cluster := self clusterKeyedByClusterName: FLSubstitutionCluster.

	(cluster isSubstitute: anObject)
		ifTrue: [ next mapAndTrace: anObject ]
		ifFalse: [
			cluster
				add: anObject
				substitutedBy: (substitutionFactory value: anObject)
				traceWith: analysis ]

]
