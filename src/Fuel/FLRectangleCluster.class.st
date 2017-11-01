"
FLRectangleCluster is an optional class that optimizes Rectangle instances, since there are a lot of instances in the system, it makes sense to optimize them. We take advantage of bytecode optimizations done by Pharo for messages like #new, #@, #x and #y. In addition, we avoid the overhead of class reshape, etc. 
"
Class {
	#name : #FLRectangleCluster,
	#superclass : #FLOptimizedObjectCluster,
	#category : 'Fuel-Clusters-Optionals'
}

{ #category : #serialize/materialize }
FLRectangleCluster >> materializeInstanceWith: aDecoder [
	"Since in Pharo #@ is associated with a special bytecode, it is faster than doing  Point basicNew"
	^ Rectangle basicNew
]

{ #category : #serialize/materialize }
FLRectangleCluster >> materializeReferencesOf: aRectangle with: aDecoder [
		
		aRectangle 
			fuelSetOrigin: aDecoder nextEncodedReference @ aDecoder nextEncodedReference	
		 	corner: aDecoder nextEncodedReference @ aDecoder nextEncodedReference
]

{ #category : #analyzing }
FLRectangleCluster >> referencesOf: aRectangle do: aBlock [

	aBlock value: aRectangle origin x.
	aBlock value: aRectangle origin y.
	aBlock value: aRectangle corner x.
	aBlock value: aRectangle corner y.
]
