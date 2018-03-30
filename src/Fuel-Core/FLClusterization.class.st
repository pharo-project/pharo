"
I manage the clusters collected during the analysis step of serialization.
"
Class {
	#name : #FLClusterization,
	#superclass : #Object,
	#instVars : [
		'baselevelBucket',
		'substitutionsBucket',
		'globalsBucket',
		'metalevelInstanceSideBucket',
		'metalevelClassSideBucket',
		'primitivesBucket',
		'postBaselevelBucket'
	],
	#category : 'Fuel-Core-Base'
}

{ #category : #'cluster buckets' }
FLClusterization >> baselevelBucket [
	
	^ baselevelBucket
]

{ #category : #accessing }
FLClusterization >> clusterBuckets [
	"Answer a new collection with all the clusters that have been collected in trace. Note that order is imporant."

	^ OrderedCollection new 
		add: globalsBucket;
		add: primitivesBucket;
		add: metalevelClassSideBucket;
		add: metalevelInstanceSideBucket;
		add: baselevelBucket;
		add: postBaselevelBucket;
		add: substitutionsBucket;
		yourself.
]

{ #category : #accessing }
FLClusterization >> clusters [
	
	^self clusterBuckets flatCollect: [:c | c ].

]

{ #category : #hooks }
FLClusterization >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #'cluster buckets' }
FLClusterization >> globalsBucket [
	
	^ globalsBucket 
]

{ #category : #initialization }
FLClusterization >> initialize [

	super initialize.
	primitivesBucket := IdentityDictionary new.
	baselevelBucket := IdentityDictionary new.
	globalsBucket := IdentityDictionary new.
	metalevelInstanceSideBucket := IdentityDictionary new.
	metalevelClassSideBucket := IdentityDictionary new.
	substitutionsBucket := IdentityDictionary new.
	postBaselevelBucket := IdentityDictionary new.

]

{ #category : #'cluster buckets' }
FLClusterization >> metalevelClassSideBucket [
	
	^ metalevelClassSideBucket
]

{ #category : #'cluster buckets' }
FLClusterization >> metalevelInstanceSideBucket [
	
	^ metalevelInstanceSideBucket 
]

{ #category : #accessing }
FLClusterization >> objectCount [

	^self clusterBuckets sum: [ :aClusterBucket | 
		aClusterBucket
			inject: 0 
			into: [ :sum :aCluster | sum + aCluster objects size ] ].
]

{ #category : #'cluster buckets' }
FLClusterization >> postBaselevelBucket [
	
	^ postBaselevelBucket
]

{ #category : #'cluster buckets' }
FLClusterization >> primitivesBucket [
	
	^ primitivesBucket
]

{ #category : #'cluster buckets' }
FLClusterization >> substitutionsBucket [
	
	^ substitutionsBucket
]
