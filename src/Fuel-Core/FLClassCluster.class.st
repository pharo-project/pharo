"
I am a cluster for storing classes in full detail.
"
Class {
	#name : 'FLClassCluster',
	#superclass : 'FLBehaviorCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'analyzing' }
FLClassCluster class >> clusterBucketIn: aClusterization [

	^aClusterization metalevelInstanceSideBucket
]

{ #category : 'serialize/materialize' }
FLClassCluster >> materializeInstanceWith: aDecoder [

	| aMetaclass |
	aMetaclass := aDecoder nextEncodedReference.
	^aMetaclass new
]

{ #category : 'serialize/materialize' }
FLClassCluster >> materializePostInstance: aClass with: aDecoder [

	| superclass name format instanceVariableNames layout comment stamp packageName packageTagName |
	superclass := aDecoder nextEncodedReference.
	packageName := aDecoder nextEncodedReference.
	packageTagName := aDecoder nextEncodedReference.
	name := aDecoder nextEncodedReference.
	format := aDecoder nextEncodedReference.
	layout := self materializeLayoutWith: aDecoder.
	instanceVariableNames := aDecoder nextEncodedString substrings: ' '.
	comment := aDecoder nextEncodedString.
	stamp := aDecoder nextEncodedString.

	aClass
		basicSuperclass: superclass;
		setName: name;
		methodDictionary: MethodDictionary new.

	self setTag: packageTagName ofPackage: packageName to: aClass.

	self
		installLayout: layout
		withFormat: format
		variableNames: instanceVariableNames
		into: aClass.

	aClass comment: comment stamp: stamp
]

{ #category : 'serialize/materialize' }
FLClassCluster >> materializeReferencesOf: aClass with: aDecoder [

	super materializeReferencesOf: aClass with: aDecoder.

	aClass classPool: aDecoder nextEncodedReference.
	aClass sharedPools: aDecoder nextEncodedReference.
	aClass environment: aDecoder nextEncodedReference.
	aClass basicSubclasses: aDecoder nextEncodedReference.
	
	Class class instSize + 1 to: aClass class instSize 
		do: [ :index | aClass instVarAt: index put: aDecoder nextEncodedReference ]
]

{ #category : 'analyzing' }
FLClassCluster >> referencesOf: aClass do: aBlock [

	super referencesOf: aClass do: aBlock.

	aBlock value: aClass classSide.
	aBlock value: aClass superclass.
	aBlock value: aClass basicClassPool.
	aBlock value: aClass basicSharedPools. 
	aBlock value: aClass package name.
	aBlock value: aClass packageTag name.
	aBlock value: aClass name.
	aBlock value: aClass format.	
	aBlock value: aClass basicEnvironment.
	aBlock value: aClass basicSubclasses.
	aBlock value: aClass classLayout class.
	
	Class class instSize + 1 to: aClass class instSize 
		do: [ :index | aBlock value: (aClass instVarAt: index) ]
]

{ #category : 'serialize/materialize' }
FLClassCluster >> serializeInstance: aClass with: anEncoder [

	anEncoder encodeReferenceTo: aClass classSide
]

{ #category : 'serialize/materialize' }
FLClassCluster >> serializePostInstance: aClass with: anEncoder [

	anEncoder encodeReferenceTo: aClass superclass.

	anEncoder
		encodeReferenceTo: aClass package name;
		encodeReferenceTo: aClass packageTag name;
		encodeReferenceTo: aClass getName;
		encodeReferenceTo: aClass format.	
	self
		serializeLayout: aClass classLayout
		with: anEncoder.
	anEncoder
		encodeString: aClass instanceVariablesString;
		encodeString: aClass comment;
		encodeString: aClass commentStamp.
		
	self flag: #todo. "The array of variable names represents a circularity problem between clusters: if we encode the reference to such array, then the cluster for arrays should be before of Class cluster. But this would disable us to serialize an array of internal classes."
]

{ #category : 'serialize/materialize' }
FLClassCluster >> serializeReferencesOf: aClass with: anEncoder [

	super serializeReferencesOf: aClass with: anEncoder.

	anEncoder encodeReferenceTo: aClass basicClassPool. 
	anEncoder encodeReferenceTo: aClass basicSharedPools. 
	anEncoder encodeReferenceTo: aClass basicEnvironment.
	anEncoder encodeReferenceTo: aClass basicSubclasses.
 
	Class class instSize + 1 to: aClass class instSize 
		do: [ :index | anEncoder encodeReferenceTo: (aClass instVarAt: index) ]
]
