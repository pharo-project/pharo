"
FLPointCluster is an optional class that optimizes Point instances, since there are a lot of instances in the system, it makes sense to optimize them. We take advantage of bytecode optimizations done by Pharo for messages like #@, #x and #y. In addition, we avoid the overhead of class reshape, etc. 
"
Class {
	#name : #FLPointCluster,
	#superclass : #FLOptimizedObjectCluster,
	#category : #'Fuel-Core-Clusters-Optionals'
}

{ #category : #'serialize/materialize' }
FLPointCluster >> materializeInstanceWith: aDecoder [
	"Since in Pharo #@ is associated with a special bytecode, it is faster than doing  Point basicNew"
	^ 0@0 
	
]

{ #category : #'serialize/materialize' }
FLPointCluster >> materializeReferencesOf: aPoint with: aDecoder [

		aPoint setX: aDecoder nextEncodedReference setY: aDecoder nextEncodedReference.	

]

{ #category : #analyzing }
FLPointCluster >> referencesOf: aPoint do: aBlock [

	aBlock value: aPoint x.
	aBlock value: aPoint y.
	
]
