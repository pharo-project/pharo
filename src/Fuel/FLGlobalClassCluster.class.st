"
I am a cluster for classes that should be present when materializing. In other words, it only serializes a reference to the class, using its name.
"
Class {
	#name : #FLGlobalClassCluster,
	#superclass : #FLGlobalCluster,
	#category : #Fuel-Clusters
}

{ #category : #serialize/materialize }
FLGlobalClassCluster >> materializeInstanceWith: aDecoder [

	^self materializeGlobalClassFrom: aDecoder 
]

{ #category : #serialize/materialize }
FLGlobalClassCluster >> serializeInstance: aClass with: anEncoder [
	
	self serializeGlobalClass: aClass on: anEncoder
]
