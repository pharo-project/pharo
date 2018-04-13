"
I clusterize objects that will be obtained on materialization via a message send to a global object. 

Explained with an example:

Suppose we have a special instance of User that represents the admin user, and it is a unique instance in the image. In case the admin user is referenced in our graph, we want to treat that object as a global. We can do that in this way:

User >> fuelAccept: aVisitor
    ^self == User admin
        ifTrue: [aVisitor visitGlobalSend: self]
        ifFalse: [super fuelAccept: aVisitor]

User >> fuelGlobalName
    ^#User

User >> fuelSelector
    ^#admin

So what will happen is that during serialization, the admin user won't be completly serialized (with all its intance variables) but instead its global name and selector are stored. Then, at materialization time, Fuel will send the selector #admin to the class User, and use what that answers as the admin user of the materialized graph.

We test this feature in FLGlobalSendSerializationTest.
"
Class {
	#name : #FLGlobalSendCluster,
	#superclass : #FLGlobalCluster,
	#instVars : [
		'globalSends'
	],
	#category : #'Fuel-Core-Clusters'
}

{ #category : #mapping }
FLGlobalSendCluster >> add: anObject name: globalName selector: selector traceWith: anAnalysis [

	self add: anObject traceWith: anAnalysis.
	globalSends
		at: anObject
		ifAbsentPut: [ Association key: globalName value: selector ]
]

{ #category : #'initialize-release' }
FLGlobalSendCluster >> initializeAnalyzing [
	super initializeAnalyzing.
	globalSends := IdentityDictionary new.
]

{ #category : #'serialize/materialize' }
FLGlobalSendCluster >> materializeInstanceWith: aDecoder [

	| global selector |
	global := (self materializeGlobalAssociationFrom: aDecoder) value.	
	selector := aDecoder nextEncodedString asSymbol.
	^ global perform: selector
]

{ #category : #'serialize/materialize' }
FLGlobalSendCluster >> serializeInstance: anObject with: anEncoder [
	
	| globalSend |
	globalSend := globalSends at: anObject.
	
	self serializeGlobalAssociationKeyed: globalSend key on: anEncoder.
	anEncoder encodeString: globalSend value.

]
