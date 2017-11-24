"
I am a generic cluster for objects without indexable variables.
"
Class {
	#name : #FLFixedObjectCluster,
	#superclass : #FLPointerObjectCluster,
	#category : #'Fuel-Clusters'
}

{ #category : #'serialize/materialize' }
FLFixedObjectCluster >> materializeInstanceWith: aDecoder [

	^theClass fuelNew
]

{ #category : #'serialize/materialize' }
FLFixedObjectCluster >> serializeInstancesStepWith: anEncoder [
	"Do nothing. I know my objects have nothing to serialize in this step."

]
